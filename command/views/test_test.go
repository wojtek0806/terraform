package views

import (
	"testing"

	"github.com/hashicorp/terraform/command/arguments"
	"github.com/hashicorp/terraform/internal/moduletest"
	"github.com/hashicorp/terraform/internal/terminal"
)

func TestTest(t *testing.T) {
	streams, close := terminal.StreamsForTesting(t)
	baseView := NewView(streams)
	view := NewTest(baseView, arguments.TestOutput{
		JUnitXMLFile: "",
	})

	results := map[string]*moduletest.Suite{}
	view.Results(results)

	output := close(t)
	gotOutput := output.All()
	wantOutput := `(map[string]*moduletest.Suite) {
}
`
	if gotOutput != wantOutput {
		t.Errorf("wrong output\ngot:\n%s\nwant:\n%s", gotOutput, wantOutput)
	}
}
