package main

import (
	"fmt"
	"go/token"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/tcard/polygo/polygo"
	"github.com/tcard/polygo/polygo/ast"
	"github.com/tcard/polygo/polygo/printer"
)

func main() {
	fromStdin := len(os.Args) == 1
	var transpilers []func() (string, string, *ast.File, io.Writer, error)
	if fromStdin {
		transpilers = append(transpilers, func() (string, string, *ast.File, io.Writer, error) {
			ast, err := polygo.Transpile(os.Stdin)
			if err != nil {
				return "stdin", "", ast, nil, err
			}
			f, err := ioutil.TempFile(os.TempDir(), "polygo-")
			return "stdin", f.Name(), ast, f, err
		})
	} else {
		for _, filename := range os.Args[1:] {
			filename := filename
			transpilers = append(transpilers, func() (string, string, *ast.File, io.Writer, error) {
				ast, err := polygo.TranspileFile(filename)
				if err != nil {
					return filename, "", ast, nil, err
				}
				dir, file := filepath.Split(filename)
				out := dir + file[:len(file)-len(filepath.Ext(file))] + ".go"
				w, err := os.Create(out)
				return filename, out, ast, w, err
			})
		}
	}

	anyError := false
	outfiles := []string{}
	for _, tsp := range transpilers {
		filename, outfile, file, w, err := tsp()
		if err != nil {
			fmt.Fprintln(os.Stderr, filename+":", err)
			anyError = true
			continue
		}
		err = printer.Fprint(w, token.NewFileSet(), file)
		if err != nil {
			fmt.Fprintln(os.Stderr, filename+":", err)
			anyError = true
		}
		outfiles = append(outfiles, outfile)
	}

	if !anyError {
		cmd := exec.Command("go", append([]string{"run"}, outfiles...)...)
		cmdout, err := cmd.CombinedOutput()
		fmt.Print(string(cmdout))
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	} else {
		os.Exit(1)
	}
}
