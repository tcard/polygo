package main

import (
	"fmt"
	"go/token"
	"strconv"
	. "github.com/tcard/polygo/ast"
)

type transformer interface {
	AST(*File) error
}

func transformAST(file *File, how string) error {
	var tfm transformer
	switch how {
	case "reflect":
		tfm = newReflectTfm()
	default:
		panic("")
	}
	return tfm.AST(file)
}

type reflectTfm struct {
	stack           []*reflectTfmFrame
	importedReflect bool
	pos             int
}

type typedecl struct {
	ident  string
	params map[string]*Ident
	vtype  Expr
}

type vardecl struct {
	ident  string
	params []string
}

type reflectTfmFrame struct {
	types      map[string]*typedecl // Parametrized types declared.
	vars       map[string]*vardecl  // Variables with instantiated parametrized types.
	stringers  map[string]int       // Parametrized type name -> tfm pos
	typeparams map[string]Expr      // Function type parameter name -> reflect.Type
	paramargs  map[string]string    // Function arguments with parametrized type
}

func newReflectTfm() *reflectTfm {
	ret := &reflectTfm{}
	ret.pushFrame()
	return ret
}

func (tfm *reflectTfm) pushFrame() {
	tfm.stack = append(tfm.stack, &reflectTfmFrame{
		types:      map[string]*typedecl{},
		vars:       map[string]*vardecl{},
		stringers:  map[string]int{},
		typeparams: map[string]Expr{},
		paramargs:  map[string]string{},
	})
}

func (tfm *reflectTfm) popFrame() {
	tfm.stack = tfm.stack[:len(tfm.stack)-1]
}

func (tfm *reflectTfm) currFrame() *reflectTfmFrame {
	return tfm.stack[len(tfm.stack)-1]
}

func (tfm *reflectTfm) lookupVar(name string) *vardecl {
	for i := len(tfm.stack) - 1; i >= 0; i-- {
		if v, ok := tfm.stack[i].vars[name]; ok {
			return v
		}
	}
	return nil
}

func (tfm *reflectTfm) lookupType(name string) *typedecl {
	for i := len(tfm.stack) - 1; i >= 0; i-- {
		if v, ok := tfm.stack[i].types[name]; ok {
			return v
		}
	}
	return nil
}

func (tfm *reflectTfm) lookupTypeparam(name string) Expr {
	for i := len(tfm.stack) - 1; i >= 0; i-- {
		if v, ok := tfm.stack[i].typeparams[name]; ok {
			return v
		}
	}
	return nil
}

func (tfm *reflectTfm) AST(file *File) error {
	for _, decl := range file.Decls {
		var err error
		switch nd := decl.(type) {
		case *GenDecl:
			err = tfm.GenDecl(nd)
		case *FuncDecl:
			err = tfm.FuncDecl(nd)
		}
		if err != nil {
			return err
		}
		tfm.pos++
	}
	for tyname, pos := range tfm.stack[0].stringers {
		file.Decls = append(file.Decls[:pos+1], append([]Decl{&FuncDecl{
			Recv: &FieldList{List: []*Field{{
				Names: []*Ident{NewIdent("v")},
				Type:  NewIdent(tyname),
			}}},
			Name: NewIdent("String"),
			Type: &FuncType{
				Params:  &FieldList{},
				Results: &FieldList{List: []*Field{{Type: NewIdent("string")}}},
			},
			Body: &BlockStmt{List: []Stmt{&ReturnStmt{
				Results: []Expr{&CallExpr{
					Fun: &SelectorExpr{
						X:   NewIdent("fmt"),
						Sel: NewIdent("Sprint"),
					},
					Args: []Expr{&SelectorExpr{
						X:   NewIdent("v"),
						Sel: NewIdent("v"),
					}},
				}},
			}}},
		}}, file.Decls[pos+1:]...)...)
	}
	if !tfm.importedReflect {
		file.Decls = append([]Decl{&GenDecl{
			Tok: token.IMPORT,
			Specs: []Spec{
				&ImportSpec{
					Path: &BasicLit{Kind: token.STRING, Value: `"reflect"`},
				},
			},
		}}, file.Decls[0:]...)
		tfm.pos++
	}
	return nil
}

func (tfm *reflectTfm) GenDecl(nd *GenDecl) error {
	switch nd.Tok {
	case token.TYPE:
		for _, spec := range nd.Specs {
			err := tfm.TypeSpec(spec.(*TypeSpec))
			if err != nil {
				return err
			}
		}
	case token.VAR:
		for _, spec := range nd.Specs {
			err := tfm.ValueSpec(spec.(*ValueSpec))
			if err != nil {
				return err
			}
		}
	case token.IMPORT:
		if tfm.importedReflect {
			break
		}
		for _, imp := range nd.Specs {
			if imp.(*ImportSpec).Path.Value == `"reflect"` {
				tfm.importedReflect = true
				break
			}
		}
	}
	return nil
}

func (tfm *reflectTfm) TypeSpec(spec *TypeSpec) error {
	if len(spec.Params) > 0 {
		tn := []*Ident{}
		tidents := map[string]*Ident{}
		for i, param := range spec.Params {
			newtn := NewIdent("t" + strconv.Itoa(i))
			tn = append(tn, newtn)
			tidents[param.Name] = newtn
		}

		tfm.ifaceTypeParams(spec.Name.Name, nil, &spec.Type, tidents)
		tfm.MakeStringer(spec.Name.Name)

		spec.Type = &StructType{
			Struct: spec.Type.Pos(),
			Fields: &FieldList{
				List: []*Field{
					{
						Names: tn,
						Type: &SelectorExpr{
							X:   NewIdent("reflect"),
							Sel: NewIdent("Type"),
						},
					},
					{Names: []*Ident{NewIdent("v")},
						Type: spec.Type,
					},
				},
			},
		}
		spec.Params = nil
	}
	return nil
}

//  *Ident, *ParenExpr, *SelectorExpr, *StarExpr, or any of the *XxxTypes

func (tfm *reflectTfm) ifaceTypeParams(tname string, vtype Expr, t *Expr, tidents map[string]*Ident) {
	intident := func(x *Ident) bool {
		_, ok := tidents[x.Name]
		return ok
	}

	switch v := (*t).(type) {
	case *Ident:
		if intident(v) {
			tfm.currFrame().types[tname] = &typedecl{
				ident:  v.Name,
				params: tidents,
				vtype:  vtype,
			}
			*t = &InterfaceType{Methods: &FieldList{}}
		}
	case *ParenExpr:
		tfm.ifaceTypeParams(tname, v, &v.X, tidents)
	case *SelectorExpr:
		tfm.ifaceTypeParams(tname, v, &v.X, tidents)
	case *StarExpr:
		tfm.ifaceTypeParams(tname, v, &v.X, tidents)
	case *ArrayType:
		tfm.ifaceTypeParams(tname, v, &v.Elt, tidents)
	case *ChanType:
		tfm.ifaceTypeParams(tname, v, &v.Value, tidents)
	case *FuncType:
		for _, param := range v.Params.List {
			tfm.ifaceTypeParams(tname, v, &param.Type, tidents)
		}
		for _, res := range v.Results.List {
			tfm.ifaceTypeParams(tname, v, &res.Type, tidents)
		}
	case *StructType:
		for _, f := range v.Fields.List {
			tfm.ifaceTypeParams(tname, v, &f.Type, tidents)
		}
	case *MapType:
		tfm.ifaceTypeParams(tname, v, &v.Key, tidents)
		tfm.ifaceTypeParams(tname, v, &v.Value, tidents)
		// TODO
		// 	type InterfaceType struct {
	}
}

func (tfm *reflectTfm) ValueSpec(spec *ValueSpec) error {
	if v, ok := spec.Type.(*TypeParamsExpr); ok {
		spec.Type = v.Type
		params := []string{}
		for _, param := range v.Params {
			params = append(params, param.Name)
		}
		for i, name := range spec.Names {
			tfm.currFrame().vars[name.Name] = &vardecl{
				ident:  name.Name,
				params: params,
			}
			if len(spec.Values) < (i + 1) {
				spec.Values = append(spec.Values, tfm.MakeCompositeLit(v, nil))
			}
		}
	}
	return nil
}

func (tfm *reflectTfm) FuncDecl(nd *FuncDecl) error {
	tfm.pushFrame()
	if nd.Recv != nil {
		for _, recv := range nd.Recv.List {
			var recvType *Expr
			if p, ok := recv.Type.(*StarExpr); ok {
				recvType = &p.X
			} else {
				recvType = &recv.Type
			}
			if pt, ok := (*recvType).(*TypeParamsExpr); ok {
				for i, param := range pt.Params {
					ident := NewIdent("t" + strconv.Itoa(i))
					tfm.currFrame().typeparams[param.Name] = &SelectorExpr{
						X:   recv.Names[0],
						Sel: ident,
					}
				}
				*recvType = pt.Type
				tfm.currFrame().vars[recv.Names[0].Name] = &vardecl{
					ident:  recv.Names[0].Name,
					params: []string{},
				}
			}
		}
	}
	body := []Stmt{}
	for i, arg := range nd.Type.Params.List {
		v, ok := arg.Type.(*Ident)
		if !ok {
			continue
		}
		typeparam := tfm.lookupTypeparam(v.Name)
		if typeparam == nil {
			continue
		}
		arg.Type = &InterfaceType{Methods: &FieldList{}}
		tvar := "__arg_t_" + strconv.Itoa(i)
		body = append(body, ArgChecker(tvar, arg.Names[0].Name, v.Name, typeparam, v)...)
	}
	paramedReturns := map[int]*Ident{}
	rm := 0
	for i := 0; nd.Type.Results != nil && i < len(nd.Type.Results.List); i++ {
		out := nd.Type.Results.List[i]
		v, ok := out.Type.(*Ident)
		if !ok {
			continue
		}
		typeparam := tfm.lookupTypeparam(v.Name)
		if typeparam == nil {
			continue
		}
		tvar := "__out_t_" + strconv.Itoa(i)
		argident := NewIdent("__out_" + strconv.Itoa(i))
		if len(out.Names) > 0 {
			argident = out.Names[0]
		}
		arg := &Field{
			Names: []*Ident{argident},
			Type:  &InterfaceType{Methods: &FieldList{}},
		}
		paramedReturns[i+rm] = argident
		nd.Type.Params.List = append(nd.Type.Params.List, arg)
		nd.Type.Results.List = append(nd.Type.Results.List[:i], nd.Type.Results.List[i+1:]...)
		i--
		rm++
		body = append(body, ArgChecker(tvar, arg.Names[0].Name, v.Name, typeparam, &StarExpr{X: v})...)
	}
	nd.Body.List = append(body, nd.Body.List...)
	tfm.Block(nd.Body)
	if len(paramedReturns) > 0 {
		for i := 0; i < len(nd.Body.List); i++ {
			switch stmt := nd.Body.List[i].(type) {
			case *ReturnStmt:
				addbody := []Stmt{}
				rm := 0
				for argi, argident := range paramedReturns {
					out := stmt.Results[argi-rm]
					// reflect.Indirect(vout).Set(reflect.ValueOf(x))
					addbody = append(addbody, &ExprStmt{X: &CallExpr{
						Fun: &SelectorExpr{
							X: &CallExpr{
								Fun: &SelectorExpr{
									X:   NewIdent("reflect"),
									Sel: NewIdent("Indirect"),
								},
								Args: []Expr{&CallExpr{
									Fun: &SelectorExpr{
										X:   NewIdent("reflect"),
										Sel: NewIdent("ValueOf"),
									},
									Args: []Expr{argident},
								}},
							},
							Sel: NewIdent("Set"),
						},
						Args: []Expr{&CallExpr{
							Fun: &SelectorExpr{
								X:   NewIdent("reflect"),
								Sel: NewIdent("ValueOf"),
							},
							Args: []Expr{out},
						}}}})
					stmt.Results = append(stmt.Results[:argi-rm], stmt.Results[argi+1-rm:]...)
					rm++
				}
				if len(stmt.Results) == 0 {
					nd.Body.List = append(nd.Body.List[:i], nd.Body.List[i+1:]...)
					i--
				}
				nd.Body.List = append(nd.Body.List, addbody...)
			}
		}
	}
	tfm.popFrame()
	return nil
}

func (tfm *reflectTfm) Block(nd *BlockStmt) error {
	for _, st := range nd.List {
		err := tfm.Stmt(st)
		if err != nil {
			return err
		}
	}
	return nil
}

func (tfm *reflectTfm) Stmt(nd Stmt) (err error) {
	switch v := nd.(type) {
	case *AssignStmt:
		return tfm.AssignStmt(v)
	case *DeclStmt:
		return tfm.GenDecl(v.Decl.(*GenDecl))
	case *ExprStmt:
		v.X, err = tfm.Expr(v.X)
		return err
	}
	return nil
}

func (tfm *reflectTfm) AssignStmt(nd *AssignStmt) error {
	for i, v := range nd.Lhs {
		expr, err := tfm.Expr(v)
		if err != nil {
			return err
		}
		nd.Lhs[i] = expr
	}
	for i, v := range nd.Rhs {
		expr, err := tfm.Expr(v)
		if err != nil {
			return err
		}
		nd.Rhs[i] = expr
	}
	return nil
}

func (tfm *reflectTfm) Expr(nd Expr) (ret Expr, err error) {
	if nd == nil {
		return
	}
	switch exp := nd.(type) {
	case *CompositeLit:
		ty, ok := exp.Type.(*TypeParamsExpr)
		if !ok {
			break
		}
		cmp := tfm.MakeCompositeLit(ty, exp.Elts)
		exp.Type = cmp.Type
		exp.Elts = cmp.Elts
	case *Ident:
		if paramarg := tfm.lookupVar(exp.Name); paramarg != nil {
			return &SelectorExpr{
				X:   exp,
				Sel: NewIdent("v"),
			}, nil
		}
	case *CallExpr:
		exp.Fun, err = tfm.Expr(exp.Fun)
		if err != nil {
			return nil, err
		}
		for i, arg := range exp.Args {
			exp.Args[i], err = tfm.Expr(arg)
			if err != nil {
				return nil, err
			}
		}
	case *BinaryExpr:
		exp.X, err = tfm.Expr(exp.X)
		if err != nil {
			return nil, err
		}
		exp.Y, err = tfm.Expr(exp.Y)
		if err != nil {
			return nil, err
		}
	case *IndexExpr:
		exp.X, err = tfm.Expr(exp.X)
		if err != nil {
			return nil, err
		}
		exp.Index, err = tfm.Expr(exp.Index)
		if err != nil {
			return nil, err
		}
	case *SliceExpr:
		exp.X, err = tfm.Expr(exp.X)
		if err != nil {
			return nil, err
		}
		exp.Low, err = tfm.Expr(exp.Low)
		if err != nil {
			return nil, err
		}
		exp.High, err = tfm.Expr(exp.High)
		if err != nil {
			return nil, err
		}
		exp.Max, err = tfm.Expr(exp.Max)
		if err != nil {
			return nil, err
		}
	}
	return nd, nil
}

func (tfm *reflectTfm) MakeStringer(ident string) {
	tfm.currFrame().stringers[ident] = tfm.pos
	tfm.pos++
}

func (tfm *reflectTfm) MakeCompositeLit(ty *TypeParamsExpr, elts []Expr) *CompositeLit {
	exp := &CompositeLit{Type: ty.Type}
	for i, param := range ty.Params {
		exp.Elts = append(exp.Elts, &KeyValueExpr{
			Key: NewIdent("t" + strconv.Itoa(i)),
			Value: &CallExpr{
				Fun: &SelectorExpr{
					X: &CallExpr{
						Fun: &SelectorExpr{
							X:   NewIdent("reflect"),
							Sel: NewIdent("TypeOf"),
						},
						Args: []Expr{
							&CallExpr{
								Fun: NewIdent("new"),
								Args: []Expr{
									param,
								},
							},
						},
					},
					Sel: NewIdent("Elem"),
				},
			},
		})
	}
	exp.Elts = append(exp.Elts, &KeyValueExpr{
		Key: NewIdent("v"),
		Value: &CompositeLit{
			Type: tfm.lookupType(exp.Type.(*Ident).Name).vtype,
			Elts: elts,
		},
	})
	return exp
}

func ArgChecker(tvar, argname, tpname string, typeparam, raw Expr) []Stmt {
	errmsg := "polygo: bad type %v for type parameter %v in argument %v, expected %v."
	var extractcond func(Expr, Expr) Expr
	extractcond = func(raw Expr, tvar Expr) Expr {
		switch v := raw.(type) {
		case *Ident:
			return &BinaryExpr{
				X:  tvar,
				Op: token.NEQ,
				Y:  typeparam,
			}
		case *StarExpr:
			return &BinaryExpr{
				X: &BinaryExpr{
					X: &CallExpr{
						Fun: &SelectorExpr{
							X:   tvar,
							Sel: NewIdent("Kind"),
						},
						Args: []Expr{},
					},
					Op: token.NEQ,
					Y: &SelectorExpr{
						X:   NewIdent("reflect"),
						Sel: NewIdent("Ptr"),
					},
				},
				Op: token.LOR,
				Y: extractcond(v.X, &CallExpr{
					Fun: &SelectorExpr{
						X:   tvar,
						Sel: NewIdent("Elem"),
					},
					Args: []Expr{},
				}),
			}
		}
		return nil
	}
	cond := extractcond(raw, NewIdent(tvar))

	return []Stmt{
		&AssignStmt{
			Lhs: []Expr{NewIdent(tvar)},
			Tok: token.DEFINE,
			Rhs: []Expr{&CallExpr{
				Fun: &SelectorExpr{
					X:   NewIdent("reflect"),
					Sel: NewIdent("TypeOf"),
				},
				Args: []Expr{NewIdent(argname)},
			}},
		},
		&IfStmt{
			Cond: cond,
			Body: &BlockStmt{List: []Stmt{
				&ExprStmt{X: &CallExpr{
					Fun: NewIdent("panic"),
					Args: []Expr{&CallExpr{
						Fun: &SelectorExpr{
							X:   NewIdent("fmt"),
							Sel: NewIdent("Sprintf"),
						},
						Args: []Expr{
							&BasicLit{Kind: token.STRING,
								Value: "`" + errmsg + "`"},
							NewIdent(tvar),
							&BasicLit{Kind: token.STRING, Value: "`" + tpname + "`"},
							&BasicLit{Kind: token.STRING, Value: "`" + argname + "`"},
							typeparam,
						},
					}},
				}},
			}},
		},
	}
}

func printNode(n interface{}) {
	fmt.Printf("%[1]T %[1]#v\n", n)
}
