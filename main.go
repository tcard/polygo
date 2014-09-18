package main

import (
	"fmt"
	"go/token"

	"github.com/tcard/polygo/parser"
)

func main() {
	s := `
	package main

	type <a>Stack []a

	var s <int>Stack

	func main() {
		a := <int>Stack{}
		var a <int>Stack
	}
	`

	file, err := parser.ParseFile(&token.FileSet{}, "", s, 0)
	_, _ = file, err
	fmt.Println("ERR", err)
	// fmt.Printf("%[1]v %[1]T\n", file.Decls[0].(*ast.GenDecl).Specs[0].(*ast.TypeSpec).Params)
	// fmt.Printf("%[1]v %[1]T\n", file.Decls[1].(*ast.GenDecl).Specs[0].(*ast.ValueSpec).Type.(*ast.TypeParamsExpr).Params)
	// fmt.Printf("%[1]v %[1]T\n", file.Decls[2].(*ast.FuncDecl).Body.List[0].(*ast.AssignStmt).Rhs[0])
}
