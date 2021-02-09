package command

import (
	"context"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/hashicorp/terraform/command/arguments"
	"github.com/hashicorp/terraform/command/views"
	"github.com/hashicorp/terraform/configs"
	"github.com/hashicorp/terraform/configs/configload"
	"github.com/hashicorp/terraform/internal/depsfile"
	"github.com/hashicorp/terraform/internal/initwd"
	"github.com/hashicorp/terraform/internal/moduletest"
	"github.com/hashicorp/terraform/internal/providercache"
	"github.com/hashicorp/terraform/tfdiags"
)

// TestCommand is the implementation of "terraform test".
type TestCommand struct {
	Meta
}

func (c *TestCommand) Run(rawArgs []string) int {
	// Parse and apply global view arguments
	common, rawArgs := arguments.ParseView(rawArgs)
	c.View.Configure(common)

	args, diags := arguments.ParseTest(rawArgs)
	view := views.NewTest(c.View, args.Output)
	if diags.HasErrors() {
		view.Diagnostics(diags)
		return 1
	}

	diags = diags.Append(tfdiags.Sourceless(
		tfdiags.Warning,
		`The "terraform test" command is experimental`,
		"We'd like to invite adventurous module authors to write integration tests for their modules using this command, but all of the behaviors of this command are currently experimental and may change based on feedback.\n\nFor more information on the testing experiment, including ongoing research goals and avenues for feedback, see:\n    https://www.terraform.io/docs/language/modules/testing-experiment.html",
	))

	ctx, cancel := c.InterruptibleContext()
	defer cancel()

	results, moreDiags := c.run(ctx, args)
	diags = diags.Append(moreDiags)
	if diags.HasErrors() {
		view.Diagnostics(diags)
		return 1
	}

	view.Results(results)
	view.Diagnostics(diags)

	if diags.HasErrors() {
		return 1
	}
	return 0
}

func (c *TestCommand) run(ctx context.Context, args arguments.Test) (results map[string]*moduletest.Suite, diags tfdiags.Diagnostics) {
	suiteNames, err := c.collectSuiteNames()
	if err != nil {
		diags = diags.Append(tfdiags.Sourceless(
			tfdiags.Error,
			"Error while searching for test configurations",
			fmt.Sprintf("While attempting to scan the 'tests' subdirectory for potential test configurations, Terraform encountered an error: %s.", err),
		))
		return nil, diags
	}

	ret := make(map[string]*moduletest.Suite, len(suiteNames))
	for _, suiteName := range suiteNames {
		if ctx.Err() != nil {
			// If the context has already failed in some way then we'll
			// halt early and report whatever's already happened.
			break
		}
		suite, moreDiags := c.runSuite(ctx, suiteName)
		diags = diags.Append(moreDiags)
		ret[suiteName] = suite
	}

	return ret, diags
}

func (c *TestCommand) runSuite(ctx context.Context, suiteName string) (*moduletest.Suite, tfdiags.Diagnostics) {
	var diags tfdiags.Diagnostics
	ret := moduletest.Suite{
		Name:       suiteName,
		Components: map[string]*moduletest.Component{},
	}

	// In order to make this initial round of "terraform test" pretty self
	// contained while it's experimental, it's largely just mimicking what
	// would happen when running the main Terraform workflow commands, which
	// comes at the expense of a few irritants that we'll hopefully resolve
	// in future iterations as the design solidifies:
	// - We need to install remote modules separately for each of the
	//   test suites, because we don't have any sense of a shared cache
	//   of modules that multiple configurations can refer to at once.
	// - We _do_ have a sense of a cache of remote providers, but it's fixed
	//   at being specifically a two-level cache (global vs. directory-specific)
	//   and so we can't easily capture a third level of "all of the test suites
	//   for this module" that sits between the two. Consequently, we need to
	//   dynamically choose between creating a directory-specific "global"
	//   cache or using the user's existing global cache, to avoid any
	//   situation were we'd be re-downloading the same providers for every
	//   one of the test suites.
	// - We need to do something a bit horrid in order to have our test
	//   provider instance persist between the plan and apply steps, because
	//   normally that is the exact opposite of what we want.
	// The above notes are here mainly as an aid to someone who might be
	// planning a subsequent phase of this R&D effort, to help distinguish
	// between things we're doing here because they are valuable vs. things
	// we're doing just to make it work without doing any disruptive
	// refactoring.

	suiteDirs, moreDiags := c.prepareSuiteDir(ctx, suiteName)
	diags = diags.Append(moreDiags)
	if diags.HasErrors() {
		// Generate a special failure representing the test initialization
		// having failed, since we therefore won'tbe able to run the actual
		// tests defined inside.
		ret.Components["(init)"] = &moduletest.Component{
			Assertions: map[string]*moduletest.Assertion{
				"(init)": {
					Outcome:     moduletest.Error,
					Description: "terraform init",
					Message:     "failed to install test suite dependencies",
					Diagnostics: diags,
				},
			},
		}
		return &ret, nil
	}

	log.Printf("[DEBUG] suite dirs %#v", suiteDirs)

	return &ret, nil
}

func (c *TestCommand) prepareSuiteDir(ctx context.Context, suiteName string) (testCommandSuiteDirs, tfdiags.Diagnostics) {
	var diags tfdiags.Diagnostics
	configDir := filepath.Join("tests", suiteName)
	log.Printf("[DEBUG] Using %s as the test configuration for suite %q", configDir, suiteName)

	suiteDirs := testCommandSuiteDirs{
		ConfigDir: configDir,
	}

	// Before we can run a test suite we need to make sure that we have all of
	// its dependencies available, so the following is essentially an
	// abbreviated form of what happens during "terraform init", with some
	// extra trickery in places.

	// First, module installation. This will include linking in the module
	// under test, but also includes grabbing the dependencies of that module
	// if it has any.
	suiteDirs.ModulesDir = filepath.Join(configDir, ".terraform", "modules")
	os.MkdirAll(suiteDirs.ModulesDir, 0755) // if this fails then we'll ignore it and let InstallModules below fail instead
	reg := c.registryClient()
	moduleInst := initwd.NewModuleInstaller(suiteDirs.ModulesDir, reg)
	_, moreDiags := moduleInst.InstallModules(configDir, true, nil)
	diags = diags.Append(moreDiags)
	if diags.HasErrors() {
		return suiteDirs, diags
	}

	// The installer puts the files in a suitable place on disk, but we
	// still need to actually load the configuration. We need to do this
	// with a separate config loader because the Meta.configLoader instance
	// is intended for interacting with the current working directory, not
	// with the test suite subdirectories.
	loader, err := configload.NewLoader(&configload.Config{
		ModulesDir: suiteDirs.ModulesDir,
		Services:   c.Services,
	})
	if err != nil {
		diags = diags.Append(tfdiags.Sourceless(
			tfdiags.Error,
			"Failed to create test configuration loader",
			fmt.Sprintf("Failed to prepare loader for test configuration %s: %s.", configDir, err),
		))
		return suiteDirs, diags
	}
	cfg, hclDiags := loader.LoadConfig(configDir)
	diags = diags.Append(hclDiags)
	if diags.HasErrors() {
		return suiteDirs, diags
	}
	suiteDirs.Config = cfg

	// With the full configuration tree available, we can now install
	// the necessary providers. We'll use a separate local cache directory
	// here, because the test configuration might have additional requirements
	// compared to the module itself.
	suiteDirs.ProvidersDir = filepath.Join(configDir, ".terraform", "providers")
	os.MkdirAll(suiteDirs.ProvidersDir, 0755) // if this fails then we'll ignore it and operations below fail instead
	localCacheDir := providercache.NewDir(suiteDirs.ProvidersDir)
	providerInst := c.providerInstaller().Clone(localCacheDir)
	if !providerInst.HasGlobalCacheDir() {
		// If the user already configured a global cache directory then we'll
		// just use it for caching the test providers too, because then we
		// can potentially reuse cache entries they already have. However,
		// if they didn't configure one then we'll still establish one locally
		// in the working directory, which we'll then share across all tests
		// to avoid downloading the same providers repeatedly.
		cachePath := filepath.Join(c.DataDir(), "testing-providers") // note this is _not_ under the suite dir
		err := os.MkdirAll(cachePath, 0755)
		// If we were unable to create the directory for any reason then we'll
		// just proceed without a cache, at the expense of repeated downloads.
		// (With that said, later installing might end up failing for the
		// same reason anyway...)
		if err == nil || os.IsExist(err) {
			cacheDir := providercache.NewDir(cachePath)
			providerInst.SetGlobalCacheDir(cacheDir)
		}
	}
	reqs, hclDiags := cfg.ProviderRequirements()
	diags = diags.Append(hclDiags)
	if diags.HasErrors() {
		return suiteDirs, diags
	}
	locks := depsfile.NewLocks() // TODO: Think about how test configurations might interact with locks, if at all
	locks, err = providerInst.EnsureProviderVersions(ctx, locks, reqs, providercache.InstallUpgrades)
	if err != nil {
		diags = diags.Append(tfdiags.Sourceless(
			tfdiags.Error,
			"Failed to install required providers",
			fmt.Sprintf("Couldn't install necessary providers for test configuration %s: %s.", configDir, err),
		))
		return suiteDirs, diags
	}

	return suiteDirs, diags
}

func (c *TestCommand) collectSuiteNames() ([]string, error) {
	items, err := ioutil.ReadDir("tests")
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}

	ret := make([]string, 0, len(items))
	for _, item := range items {
		if !item.IsDir() {
			continue
		}
		name := item.Name()
		suitePath := filepath.Join("tests", name)
		tfFiles, err := filepath.Glob(filepath.Join(suitePath, "*.tf"))
		if err != nil {
			// We'll just ignore it and treat it like a dir with no .tf files
			tfFiles = nil
		}
		tfJSONFiles, err := filepath.Glob(filepath.Join(suitePath, "*.tf.json"))
		if err != nil {
			// We'll just ignore it and treat it like a dir with no .tf.json files
			tfJSONFiles = nil
		}
		if (len(tfFiles) + len(tfJSONFiles)) == 0 {
			// Not a test suite, then.
			continue
		}
		ret = append(ret, name)
	}

	return ret, nil
}

func (c *TestCommand) Help() string {
	helpText := `
Usage: terraform test [options]

  This is an experimental command to help with automated integration
  testing of shared modules. The usage and behavior of this command is
  likely to change in breaking ways in subsequent releases, as we
  are currently using this command primarily for research purposes.

  In its current experimental form, "test" will look under the current
  working directory for a subdirectory called "tests", and then within
  that directory search for one or more subdirectories that contain
  ".tf" or ".tf.json" files. For any that it finds, it will perform
  Terraform operations similar to the following sequence of commands
  in each of those directories:
      terraform validate
      terraform apply
      terraform destroy

  The test configurations should not declare any input variables and
  should at least contain a call to the module being tested, which
  will always be available at the path ../.. due to the expected
  filesystem layout.

  The tests are considered to be successful if all of the above steps
  succeed.

  Test configurations may optionally include uses of the special
  built-in test provider terraform.io/builtin/test, which allows
  writing explicit test assertions which must also all pass in order
  for the test run to be considered successful.

  This initial implementation is intended as a minimally-viable
  product to use for further research and experimentation, and in
  particular it currently lacks the following capabilities that we
  expect to consider in later iterations, based on feedback:
    - Testing of subsequent updates to existing infrastructure,
      where currently it only supports initial creation and
      then destruction.
    - Testing top-level modules that are intended to be used for
      "real" environments, which typically have hard-coded values
      that don't permit creating a separate "copy" for testing.
    - Some sort of support for unit test runs that don't interact
      with remote systems at all, e.g. for use in checking pull
      requests from untrusted contributors.

  In the meantime, we'd like to hear feedback from module authors
  who have tried writing some experimental tests for their modules
  about what sorts of tests you were able to write, what sorts of
  tests you weren't able to write, and any tests that you were
  able to write but that were difficult to model in some way.

Options:

  -compact-warnings  Use a more compact representation for warnings, if
                     this command produces only warnings and no errors.

  -junit-xml=FILE    In addition to the usual output, also write test
                     results to the given file path in JUnit XML format.
                     This format is commonly supported by CI systems, and
                     they typically expect to be given a filename to search
                     for in the test workspace after the test run finishes.

  -no-color          Don't include virtual terminal formatting sequences in
                     the output.
`
	return strings.TrimSpace(helpText)
}

func (c *TestCommand) Synopsis() string {
	return "Experimental support for module integration testing"
}

type testCommandSuiteDirs struct {
	ConfigDir    string
	ModulesDir   string
	ProvidersDir string

	Config *configs.Config
}
