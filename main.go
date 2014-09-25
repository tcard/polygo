package main

import (
	"fmt"
	"go/token"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/tcard/polygo/parser"
	"github.com/tcard/polygo/printer"
)

func main() {
	src, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	ast, err := parser.ParseFile(&token.FileSet{}, "", string(src), 0)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	err = transformAST(ast, "reflect")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	dir, file := filepath.Split(os.Args[1])
	out := dir + file[:len(file)-len(filepath.Ext(file))] + ".go"
	w, _ := os.Create(out)

	err = printer.Fprint(w, token.NewFileSet(), ast)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	cmd := exec.Command("go", "run", out)
	cmdout, err := cmd.CombinedOutput()
	fmt.Print(string(cmdout))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
