// Package polygo implements translation from Polygo source code to Go source code.
package polygo

import (
	"io"
	"io/ioutil"
	"os"
	"strings"

	"go/token"

	"github.com/tcard/polygo/polygo/ast"
	polyparser "github.com/tcard/polygo/polygo/parser"
	"github.com/tcard/polygo/polygo/reflect"
)

func TranspileFile(filename string) (*ast.File, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	return Transpile(f)
}

func TranspileString(src string) (*ast.File, error) {
	buf := strings.NewReader(src)
	return Transpile(buf)
}

func Transpile(r io.Reader) (*ast.File, error) {
	src, err := ioutil.ReadAll(r)
	if err != nil {
		return nil, err
	}
	ast, err := polyparser.ParseFile(&token.FileSet{}, "", string(src), 0)
	if err != nil {
		return nil, err
	}
	return TranspileAST(ast, "reflect")
}

func TranspileAST(ast *ast.File, how string) (*ast.File, error) {
	var tsp transpiler
	switch how {
	case "reflect":
		tsp = reflect.NewReflectTsp()
	default:
		panic("")
	}
	return tsp.TranspileAST(ast)
}

type transpiler interface {
	TranspileAST(*ast.File) (*ast.File, error)
}
