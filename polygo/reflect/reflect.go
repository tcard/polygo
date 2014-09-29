// Package reflect implements Polygo transpilation by reflection means.
package reflect

/*
Transformations:

1. A type T with parameters p0, p1, p2... and definition X will be given a new definition: a struct
  with fields t0, t1, t2... that will hold reflect.Types, and a field d, which will hold X with
  each appearance of p0, p1, p2... replaced by interface{}.
  1. The original will be kept in a data structure so we can typecheck later.
2. A function f with type parameters p0, p1, p2... will:
  1. Add t0, t1, t2... arguments with reflect.Type types to the beginning of the argument list.
     Every argument whose type refers to the type parameters will have its type turned into
     interface{}.
  2. Every result r0, r1... that refers to a type parameter will be removed and a new argument added
     to the argument list with type interface{}. Those will need to be pointers of the defined type.
        func <a>f() (map[a]int, error) --> func <a>f(out *map[a]int) error
  3. For those added arguments, and for the receiver if its type is parameterized, some checks
     using reflection will be added at the top of the function body for the passed to be coherent
     with the defined schema.
  4. Every return statement will only return the results not using the type parameters. r0, r1...
     will be then set using reflection.
  5. Parameters with parameterized types (including the receiver if its type is parameterized) will
     be handled with reflection.
3. Inside a function body with active type parameters p0, p1, p2...:
  1. Constructing a value of a parameterized type will be transformed to a construction of the
     transpiled struct. t0, t1, t2... will be given reflect.Types of the passed types. In v, we'll
     put the previous RHS of the construction, with the type parameters turned into interface{} to
     match the converted defintion.
    	m := <string, int>OrderedMap{m: map[string]int{}}
    	m := OrderedMap{v: struct{m: map[interface{}]interface{}{}}
  2. If construction is empty (e. g. var st <int>Stack), we'll generate a empty value for .v with
     reflection.
  3. Any reference to a value whose type is parameterized will be turned into a selection of its
     .v. _Except_ for methods.
  4. A call to a type parameterized function that uses type parameters as result will be turn into
     a anonymous function that will be called immediately.


TODO

type IntStack <int>Stack

*/

import (
	"fmt"
	"go/token"
	"strconv"

	. "github.com/tcard/polygo/polygo/ast"
)

type reflectTsp struct {
	scopes          []*reflectTspScope
	importedReflect bool
	importedFmt     bool
	pos             int
}

type typeDecl struct {
	ident       string
	params      map[string]string // T -> t0, U -> t1...
	definition  Expr              // map[interface{}]interface{}
	originalDef Expr              // map[T]U
	pos         int
}

type varDecl struct {
	ident        string
	type_        Expr
	passedParams []Expr
}

type reflectTspScope struct {
	// Parametrized types available, by identifier. When making an instance of a parametrized type,
	// we will pass the type arguments as reflect.Type values to the generated struct.
	paramedTypes map[string]*typeDecl

	// Variables available, by identifier. We need them to typecheck. If their types are
	// parametrized, every mention to them needs to be converted to access their .v field.
	// (Except when calling methods on them.)
	vars map[string]*varDecl

	// The type parameters active within the current scope (ie. passed to the current function).
	// Values are expressions of reflect.Type values.
	passedParams map[string]Expr
}

func NewReflectTsp() *reflectTsp {
	ret := &reflectTsp{}
	ret.pushFrame()
	return ret
}

func (tsp *reflectTsp) pushFrame() {
	tsp.scopes = append(tsp.scopes, &reflectTspScope{
		paramedTypes: map[string]*typeDecl{},
		vars:         map[string]*varDecl{},
		passedParams: map[string]Expr{},
	})
}

func (tsp *reflectTsp) popFrame() {
	tsp.scopes = tsp.scopes[:len(tsp.scopes)-1]
}

func (tsp *reflectTsp) currFrame() *reflectTspScope {
	return tsp.scopes[len(tsp.scopes)-1]
}

func (tsp *reflectTsp) lookupVar(name string) *varDecl {
	for i := len(tsp.scopes) - 1; i >= 0; i-- {
		if v, ok := tsp.scopes[i].vars[name]; ok {
			return v
		}
	}
	return nil
}

func (tsp *reflectTsp) lookupType(name string) *typeDecl {
	for i := len(tsp.scopes) - 1; i >= 0; i-- {
		if v, ok := tsp.scopes[i].paramedTypes[name]; ok {
			return v
		}
	}
	return nil
}

func (tsp *reflectTsp) lookupPassedParam(name string) Expr {
	for i := len(tsp.scopes) - 1; i >= 0; i-- {
		if v, ok := tsp.scopes[i].passedParams[name]; ok {
			return v
		}
	}
	return nil
}

func (tsp *reflectTsp) TranspileAST(file *File) (*File, error) {
	for _, decl := range file.Decls {
		var err error
		switch nd := decl.(type) {
		case *GenDecl:
			err = tsp.GenDecl(nd)
		case *FuncDecl:
			err = tsp.FuncDecl(nd)
		}
		if err != nil {
			return nil, err
		}
		tsp.pos++
	}
	for tyname, tydecl := range tsp.scopes[0].paramedTypes {
		file.Decls = append(file.Decls[:tydecl.pos+1], append([]Decl{&FuncDecl{
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
		}}, file.Decls[tydecl.pos+1:]...)...)
	}
	if !tsp.importedReflect {
		file.Decls = append([]Decl{&GenDecl{
			Tok: token.IMPORT,
			Specs: []Spec{
				&ImportSpec{
					Path: &BasicLit{Kind: token.STRING, Value: `"reflect"`},
				},
			},
		}}, file.Decls[0:]...)
		tsp.pos++
	}
	if !tsp.importedFmt && len(tsp.scopes[0].paramedTypes) > 0 {
		file.Decls = append([]Decl{&GenDecl{
			Tok: token.IMPORT,
			Specs: []Spec{
				&ImportSpec{
					Path: &BasicLit{Kind: token.STRING, Value: `"fmt"`},
				},
			},
		}}, file.Decls[0:]...)
		tsp.pos++
	}
	return file, nil
}

func (tsp *reflectTsp) GenDecl(nd *GenDecl) error {
	switch nd.Tok {
	case token.TYPE:
		for i, _ := range nd.Specs {
			err := tsp.TypeSpec(&nd.Specs[i])
			if err != nil {
				return err
			}
		}
	case token.VAR:
		for _, spec := range nd.Specs {
			err := tsp.ValueSpec(spec.(*ValueSpec))
			if err != nil {
				return err
			}
		}
	case token.IMPORT:
		for _, imp := range nd.Specs {
			if tsp.importedReflect && tsp.importedFmt {
				break
			}
			switch imp.(*ImportSpec).Path.Value {
			case `"reflect"`, "`reflect`":
				tsp.importedReflect = true
			case `"fmt"`, "`fmt`":
				tsp.importedFmt = true
			}
		}
	}
	return nil
}

func (tsp *reflectTsp) TypeSpec(ispec *Spec) error {
	switch spec := (*ispec).(type) {
	case *ParamedTypeSpec:
		// 1.
		paramIdents := map[string]string{} // T -> t0, U -> t1
		tn := []*Ident{}
		for i, param := range spec.Params {
			newtn := NewIdent("t" + strconv.Itoa(i))
			tn = append(tn, newtn)
			paramIdents[param.Name] = newtn.Name
		}

		prevDef := spec.Type
		spec.Type = tsp.replaceTypeParams(spec.Type, paramIdents)

		tsp.currFrame().paramedTypes[spec.Name.Name] = &typeDecl{
			ident:       spec.Name.Name,
			params:      paramIdents,
			definition:  spec.Type,
			originalDef: prevDef, // 1.1
		}

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
		*ispec = &spec.TypeSpec
	}
	return nil
}

// Find any occurrence of paramIdents inside typeDef, replacing it with interface{}.
func (tsp *reflectTsp) replaceTypeParams(typeDef Expr, paramIdents map[string]string) Expr {
	isTypeParam := func(x *Ident) bool {
		_, ok := paramIdents[x.Name]
		return ok
	}

	switch v := typeDef.(type) {
	case *Ident:
		if isTypeParam(v) {
			return &InterfaceType{Methods: &FieldList{}}
		}
		return &Ident{Name: v.Name, NamePos: v.NamePos, Obj: v.Obj}
	case *ParenExpr:
		return &ParenExpr{
			Lparen: v.Lparen, Rparen: v.Rparen,
			X: tsp.replaceTypeParams(v.X, paramIdents),
		}
	case *SelectorExpr:
		return &SelectorExpr{
			Sel: v.Sel,
			X:   tsp.replaceTypeParams(v.X, paramIdents),
		}
	case *StarExpr:
		return &StarExpr{
			Star: v.Star,
			X:    tsp.replaceTypeParams(v.X, paramIdents),
		}
	case *ArrayType:
		return &ArrayType{
			Lbrack: v.Lbrack, Len: v.Len,
			Elt: tsp.replaceTypeParams(v.Elt, paramIdents),
		}
	case *ChanType:
		return &ChanType{
			Arrow: v.Arrow, Begin: v.Begin, Dir: v.Dir,
			Value: tsp.replaceTypeParams(v.Value, paramIdents),
		}
	case *FuncType:
		ret := &FuncType{
			Func:    v.Func,
			Params:  &FieldList{Opening: v.Params.Opening, Closing: v.Params.Closing},
			Results: &FieldList{Opening: v.Results.Opening, Closing: v.Results.Closing},
		}
		for _, param := range v.Params.List {
			ret.Params.List = append(ret.Params.List, &Field{
				Doc: param.Doc, Names: param.Names, Tag: param.Tag, Comment: param.Comment,
				Type: tsp.replaceTypeParams(param.Type, paramIdents),
			})
		}
		for _, res := range v.Results.List {
			ret.Results.List = append(ret.Results.List, &Field{
				Doc: res.Doc, Names: res.Names, Tag: res.Tag, Comment: res.Comment,
				Type: tsp.replaceTypeParams(res.Type, paramIdents),
			})
		}
		return ret
	case *StructType:
		ret := &StructType{
			Struct: v.Struct, Incomplete: v.Incomplete,
			Fields: &FieldList{Opening: v.Fields.Opening, Closing: v.Fields.Closing},
		}
		for _, f := range v.Fields.List {
			ret.Fields.List = append(ret.Fields.List, &Field{
				Doc: f.Doc, Names: f.Names, Tag: f.Tag, Comment: f.Comment,
				Type: tsp.replaceTypeParams(f.Type, paramIdents),
			})
		}
		return ret
	case *MapType:
		return &MapType{
			Key:   tsp.replaceTypeParams(v.Key, paramIdents),
			Map:   v.Map,
			Value: tsp.replaceTypeParams(v.Value, paramIdents),
		}
		// TODO
		// 	type InterfaceType struct {
	case *TypeParamsExpr:
		return &TypeParamsExpr{
			Params: v.Params,
			Type:   tsp.replaceTypeParams(v.Type, paramIdents),
		}
	}
	return typeDef
}

func (tsp *reflectTsp) ValueSpec(spec *ValueSpec) error {
	if v, ok := spec.Type.(*TypeParamsExpr); ok {
		spec.Type = v.Type
		params := []Expr{}
		for _, param := range v.Params {
			params = append(params, param)
		}
		for i, name := range spec.Names {
			tsp.currFrame().vars[name.Name] = &varDecl{
				ident:        name.Name,
				passedParams: params,
			}
			if len(spec.Values) < (i + 1) {
				spec.Values = append(spec.Values, tsp.MakeCompositeLit(v, nil))
			}
		}
	}
	return nil
}

func (tsp *reflectTsp) FuncDecl(nd *FuncDecl) error {
	tsp.pushFrame()
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
					tsp.currFrame().passedParams[param.(*Ident).Name] = &SelectorExpr{
						X:   recv.Names[0],
						Sel: ident,
					}
				}
				*recvType = pt.Type
				tsp.currFrame().vars[recv.Names[0].Name] = &varDecl{
					ident:        recv.Names[0].Name,
					passedParams: []Expr{},
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
		typeparam := tsp.lookupPassedParam(v.Name)
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
		typeparam := tsp.lookupPassedParam(v.Name)
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
	tsp.Block(nd.Body)
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
				nd.Body.List = append(nd.Body.List[:i], append(addbody, nd.Body.List[i:]...)...)
				i += len(addbody)
			}
		}
	}
	tsp.popFrame()
	return nil
}

func (tsp *reflectTsp) Block(nd *BlockStmt) error {
	for _, st := range nd.List {
		err := tsp.Stmt(st)
		if err != nil {
			return err
		}
	}
	return nil
}

func (tsp *reflectTsp) Stmt(nd Stmt) (err error) {
	switch v := nd.(type) {
	case *AssignStmt:
		return tsp.AssignStmt(v)
	case *DeclStmt:
		return tsp.GenDecl(v.Decl.(*GenDecl))
	case *ExprStmt:
		v.X, err = tsp.Expr(v.X)
		return err
	}
	return nil
}

func (tsp *reflectTsp) AssignStmt(nd *AssignStmt) error {
	for i, v := range nd.Lhs {
		expr, err := tsp.Expr(v)
		if err != nil {
			return err
		}
		nd.Lhs[i] = expr
	}
	for i, v := range nd.Rhs {
		expr, err := tsp.Expr(v)
		if err != nil {
			return err
		}
		nd.Rhs[i] = expr
	}
	return nil
}

func (tsp *reflectTsp) Expr(nd Expr) (ret Expr, err error) {
	if nd == nil {
		return
	}
	switch exp := nd.(type) {
	case *CompositeLit:
		ty, ok := exp.Type.(*TypeParamsExpr)
		if !ok {
			break
		}
		cmp := tsp.MakeCompositeLit(ty, exp.Elts)
		exp.Type = cmp.Type
		exp.Elts = cmp.Elts
	case *Ident:
		if paramarg := tsp.lookupVar(exp.Name); paramarg != nil {
			return &SelectorExpr{
				X:   exp,
				Sel: NewIdent("v"),
			}, nil
		}
	case *CallExpr:
		exp.Fun, err = tsp.Expr(exp.Fun)
		if err != nil {
			return nil, err
		}
		for i, arg := range exp.Args {
			exp.Args[i], err = tsp.Expr(arg)
			if err != nil {
				return nil, err
			}
		}
	case *BinaryExpr:
		exp.X, err = tsp.Expr(exp.X)
		if err != nil {
			return nil, err
		}
		exp.Y, err = tsp.Expr(exp.Y)
		if err != nil {
			return nil, err
		}
	case *IndexExpr:
		exp.X, err = tsp.Expr(exp.X)
		if err != nil {
			return nil, err
		}
		exp.Index, err = tsp.Expr(exp.Index)
		if err != nil {
			return nil, err
		}
	case *SliceExpr:
		exp.X, err = tsp.Expr(exp.X)
		if err != nil {
			return nil, err
		}
		exp.Low, err = tsp.Expr(exp.Low)
		if err != nil {
			return nil, err
		}
		exp.High, err = tsp.Expr(exp.High)
		if err != nil {
			return nil, err
		}
		exp.Max, err = tsp.Expr(exp.Max)
		if err != nil {
			return nil, err
		}
	case *SelectorExpr:
		exp.X, err = tsp.Expr(exp.X)
		if err != nil {
			return nil, err
		}
	}
	return nd, nil
}

func (tsp *reflectTsp) MakeCompositeLit(ty *TypeParamsExpr, elts []Expr) *CompositeLit {
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
			Type: tsp.lookupType(exp.Type.(*Ident).Name).definition,
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
