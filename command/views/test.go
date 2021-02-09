package views

import (
	"github.com/davecgh/go-spew/spew"
	"github.com/hashicorp/terraform/command/arguments"
	"github.com/hashicorp/terraform/internal/moduletest"
	"github.com/hashicorp/terraform/internal/terminal"
	"github.com/hashicorp/terraform/tfdiags"
)

// Test is the view interface for the "terraform test" command.
type Test interface {
	// Results presents the given test results.
	Results(map[string]*moduletest.Suite) tfdiags.Diagnostics

	// Diagnostics is for reporting warnings or errors that occurred with the
	// mechanics of running tests. For this command in particular, some
	// errors are considered to be test failures rather than mechanism failures,
	// and so those will be reported via Results rather than via Diagnostics.
	Diagnostics(tfdiags.Diagnostics)
}

// NewTest returns an implementation of Test configured to respect the
// settings described in the given arguments.
func NewTest(base *View, args arguments.TestOutput) Test {
	return &testHuman{
		streams:         base.streams,
		showDiagnostics: base.Diagnostics,
		junitXMLFile:    args.JUnitXMLFile,
	}
}

type testHuman struct {
	// This is the subset of functionality we need from the base view.
	streams         *terminal.Streams
	showDiagnostics func(diags tfdiags.Diagnostics)

	// If junitXMLFile is not empty then results will be written to
	// the given file path in addition to the usual output.
	junitXMLFile string
}

func (v *testHuman) Results(results map[string]*moduletest.Suite) tfdiags.Diagnostics {
	// TODO: Something more appropriate than this
	v.streams.Print(spew.Sdump(results))

	if v.junitXMLFile != "" {
		// TODO: Also write JUnit XML to the given file
	}

	return nil
}

func (v *testHuman) Diagnostics(diags tfdiags.Diagnostics) {
	if len(diags) == 0 {
		return
	}
	v.showDiagnostics(diags)
}
