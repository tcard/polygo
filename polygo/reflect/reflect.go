// Package reflect implements Polygo transpilation by reflection means.
package reflect

/*
Transformations:

1. A type T with parameters p0, p1, p2... and definition X will be given a new definition: a struct
  with fields t0, t1, t2... that will hold reflect.Types, and a field d, which will hold X with
  each appearance of p0, p1, p2... replaced by interface{}.
  1. The original will be kept in a data structure so we can typecheck later.
2. A function f with type parameters p0, p1, p2... will:
  TOOO 1. Add t0, t1, t2... arguments with reflect.Type types to the beginning of the argument list.
     Every argument whose type refers to the type parameters will have its type turned into
     interface{}.
  2. Every result r0, r1... that refers to a type parameter will be removed and a new argument added
     to the argument list with type interface{}. Those will need to be pointers of the defined type.
        func <a>f() (map[a]int, error) --> func <a>f(out *map[a]int) error
  3. For those added arguments, and for the receiver if its type is parameterized, and for the new
  	 out arguments, some checks using reflection will be added at the top of the function body for
  	 the passed to be coherent with the defined schema.
  4. Every return statement will only return the results not using the type parameters. r0, r1...
     will be then set using reflection.
  TODO 5. Parameters with parameterized types (including the receiver if its type is parameterized) will
     be handled with reflection.
3. Inside a function body with active type parameters p0, p1, p2...:
  1. Constructing a value of a parameterized type will be transformed to a construction of the
     transpiled struct. t0, t1, t2... will be given reflect.Types of the passed types.
    1. In v, we'll put the previous RHS of the construction, with the type parameters turned into
       interface{} to match the converted defintion.
    	m := <string, int>OrderedMap{m: map[string]int{}}
    	m := OrderedMap{v: struct{m: map[interface{}]interface{}{}}
  TODO 2. If construction is empty (e. g. var st <int>Stack), we'll generate a empty value for .v with
     reflection.
  3. Any reference to a value whose type is parameterized will be turned into a selection of its
     .v.
    1. _Except_ for methods.
  4. A call to a type parameterized function that uses type parameters as result will be turn into
     a anonymous function that will be called immediately.


TODO

type IntStack <int>Stack

- Typechecking all over the place.

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
	ident          string
	params         []string // T, U...
	definition     Expr     // map[interface{}]interface{}
	originalDef    Expr     // map[T]U
	pos            int
	methods        map[string]typeDeclMethod
	paramedResults []int
}

type typeDeclMethod struct {
	formalTypeParams []string
	fun              *FuncType
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
	activeParams map[string]Expr
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
		activeParams: map[string]Expr{},
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

func (tsp *reflectTsp) lookupActiveParam(name string) Expr {
	for i := len(tsp.scopes) - 1; i >= 0; i-- {
		if v, ok := tsp.scopes[i].activeParams[name]; ok {
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
	spec, ok := (*ispec).(*ParamedTypeSpec)
	if !ok {
		return nil
	}
	// 1.
	paramIdents := []string{}
	tn := []*Ident{}
	for i, param := range spec.Params {
		newtn := NewIdent("t" + strconv.Itoa(i))
		tn = append(tn, newtn)
		paramIdents = append(paramIdents, param.Name)
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
	return nil
}

// Find any occurrence of paramIdents inside typeDef, replacing it with interface{}.
func (tsp *reflectTsp) replaceTypeParams(typeDef Expr, paramIdents []string) Expr {
	isTypeParam := func(x *Ident) bool {
		for _, param := range paramIdents {
			if x.Name == param {
				return true
			}
		}
		return false
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
				type_:        spec.Type,
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
	if nd.Recv != nil {
	}
	tsp.pushFrame()
	defer tsp.popFrame()

	// 2.3
	if nd.Recv != nil {
		for _, recv := range nd.Recv.List {
			var recvType *Expr
			if p, ok := recv.Type.(*StarExpr); ok {
				recvType = &p.X
			} else {
				recvType = &recv.Type
			}
			var recvName string
			formalParams := []string{}
			if pt, ok := (*recvType).(*TypeParamsExpr); ok {
				for i, param := range pt.Params {
					ident := NewIdent("t" + strconv.Itoa(i))
					tsp.currFrame().activeParams[param.(*Ident).Name] = &SelectorExpr{
						X:   recv.Names[0],
						Sel: ident,
					}
					formalParams = append(formalParams, param.(*Ident).Name)
				}
				*recvType = pt.Type
				tsp.currFrame().vars[recv.Names[0].Name] = &varDecl{
					ident:        recv.Names[0].Name,
					passedParams: []Expr{},
				}
				recvName = pt.Type.(*Ident).Name
			} else if ident, ok := (*recvType).(*Ident); ok {
				recvName = ident.Name
			} else {
				panic("")
			}
			t := tsp.scopes[0].paramedTypes[recvName]
			if t == nil {
				continue
			}
			if t.methods == nil {
				t.methods = map[string]typeDeclMethod{}
			}
			// Got to copy nd because it'll be mutated later.
			method := typeDeclMethod{
				formalTypeParams: formalParams,
				fun: &FuncType{
					Func:    nd.Type.Func,
					Params:  &FieldList{List: []*Field{}},
					Results: nil,
				},
			}
			for _, p := range nd.Type.Params.List {
				method.fun.Params.List = append(method.fun.Params.List, p)
			}
			if nd.Type.Results != nil {
				method.fun.Results = &FieldList{List: []*Field{}}
				for _, r := range nd.Type.Results.List {
					method.fun.Results.List = append(method.fun.Results.List, r)
				}
			}
			t.methods[nd.Name.Name] = method
		}
	}

	// 2.3
	body := []Stmt{}
	for i, arg := range nd.Type.Params.List {
		v, ok := arg.Type.(*Ident)
		if !ok {
			continue
		}
		typeparam := tsp.lookupActiveParam(v.Name)
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
		// 2.2
		typeparam := tsp.lookupActiveParam(v.Name)
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
		// 2.3
		body = append(body, ArgChecker(tvar, arg.Names[0].Name, v.Name, typeparam, &StarExpr{X: v})...)
	}
	nd.Body.List = append(body, nd.Body.List...)
	tsp.Block(nd.Body)

	// 2.4
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
	return nil
}

func (tsp *reflectTsp) Block(nd *BlockStmt) error {
	// 3.
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
		v.X = tsp.Expr(v.X)
		return nil
	}
	return nil
}

func (tsp *reflectTsp) AssignStmt(nd *AssignStmt) error {
	for i, v := range nd.Lhs {
		nd.Lhs[i] = tsp.Expr(v)
	}
	for i, v := range nd.Rhs {
		if clit, ok := v.(*CompositeLit); ok {
			if tpexp, ok := clit.Type.(*TypeParamsExpr); ok {
				tsp.currFrame().vars[nd.Lhs[i].(*Ident).Name] = &varDecl{
					ident:        nd.Lhs[i].(*Ident).Name, // TODO: non-ident LHS
					type_:        tpexp.Type,
					passedParams: tpexp.Params,
				}
			}
		}
		nd.Rhs[i] = tsp.Expr(v)
	}
	return nil
}

func (tsp *reflectTsp) Expr(nd Expr) Expr {
	if nd == nil {
		return nd
	}
	switch exp := nd.(type) {
	case *CompositeLit:
		// 3.1
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
			}
		}
	case *CallExpr:
		return tsp.CallExpr(exp)
	case *BinaryExpr:
		exp.X = tsp.Expr(exp.X)
		exp.Y = tsp.Expr(exp.Y)
	case *IndexExpr:
		exp.X = tsp.Expr(exp.X)
		exp.Index = tsp.Expr(exp.Index)
	case *SliceExpr:
		exp.X = tsp.Expr(exp.X)
		exp.Low = tsp.Expr(exp.Low)
		exp.High = tsp.Expr(exp.High)
		exp.Max = tsp.Expr(exp.Max)
	case *SelectorExpr:
		// 3.3.1
		if t, _ := tsp.paramedTypeOfExpr(exp.X); t == nil {
			exp.X = tsp.Expr(exp.X)
		}
	}
	return nd
}

func (tsp *reflectTsp) CallExpr(exp *CallExpr) Expr {
	exp.Fun = tsp.Expr(exp.Fun)
	for i, arg := range exp.Args {
		exp.Args[i] = tsp.Expr(arg)
	}
	// 3.4
	methExp, ok := exp.Fun.(*SelectorExpr)
	if !ok {
		return exp
	}
	type_, passedParams := tsp.paramedTypeOfExpr(methExp.X)
	if type_ == nil || type_.methods == nil {
		return exp
	}
	method := type_.methods[methExp.Sel.Name]
	if method.fun.Results == nil {
		return exp
	}
	type result struct {
		name      string
		type_     Expr
		isParamed bool
	}
	results := []result{}
	anyParamed := false
	i := 0
	for _, res := range method.fun.Results.List {
		var names []*Ident
		if len(res.Names) > 0 {
			names = res.Names
		} else {
			names = []*Ident{NewIdent("out" + strconv.Itoa(i))}
		}
		for _, name := range names {
			var restype Expr
			for pi, p := range method.formalTypeParams {
				if p == res.Type.(*Ident).Name {
					restype = passedParams[pi]
					break
				}
			}
			isParamed := restype != nil
			if isParamed {
				anyParamed = true
			} else {
				restype = res.Type
			}
			results = append(results, result{
				name:      name.Name,
				type_:     restype,
				isParamed: isParamed,
			})
		}
		i++
	}
	if anyParamed {
		resultList := []*Field{}
		returnIdents := []Expr{}
		lhs := []Expr{}
		var specs []Spec
		for _, r := range results {
			resultList = append(resultList, &Field{
				Type: r.type_,
			})
			if r.isParamed {
				specs = append(specs, &ValueSpec{
					Names: []*Ident{NewIdent(r.name)},
					Type:  r.type_,
				})
				exp.Args = append(exp.Args, &UnaryExpr{
					Op: token.AND,
					X:  NewIdent(r.name),
				})
			} else {
				lhs = append(lhs, NewIdent(r.name))
			}
			returnIdents = append(returnIdents, NewIdent(r.name))
		}
		var doCall Stmt = &ExprStmt{exp}
		if len(lhs) > 0 {
			doCall = &AssignStmt{
				Lhs: lhs,
				Tok: token.DEFINE,
				Rhs: []Expr{exp},
			}
		}
		return &CallExpr{Fun: &FuncLit{
			Type: &FuncType{
				Results: &FieldList{List: resultList},
			},
			Body: &BlockStmt{
				List: []Stmt{
					&DeclStmt{Decl: &GenDecl{
						Tok:   token.VAR,
						Specs: specs,
					}},
					doCall,
					&ReturnStmt{
						Results: returnIdents,
					},
				},
			},
		}}
	}
	return exp
}

// Within a scope, returns the type of an expression if it is parametrized and the passed params
// to it.
func (tsp *reflectTsp) paramedTypeOfExpr(exp Expr) (decl *typeDecl, passedParams []Expr) {
	switch vexp := exp.(type) {
	case *Ident:
		var_ := tsp.lookupVar(vexp.Name)
		if var_ == nil {
			return
		}
		type_, ok := var_.type_.(*Ident)
		if !ok {
			return
		}
		return tsp.lookupType(type_.Name), var_.passedParams
		// TODO
	}
	return
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

	typeDecl := tsp.lookupType(exp.Type.(*Ident).Name)

	// 3.1.1
	for _, elt := range elts {
		kvelt, ok := elt.(*KeyValueExpr)
		if !ok {
			continue
		}
		eltval := kvelt.Value
		var toReplace *Expr
		switch v := eltval.(type) {
		case *CallExpr:
			funIdent, ok := v.Fun.(*Ident)
			if !ok || funIdent.Name != "make" {
				// Not a make in the value. Nothing we can do about that.
				continue
			}
			toReplace = &v.Args[0]
		case *CompositeLit:
			toReplace = &v.Type
		}

		structDef, ok := typeDecl.definition.(*StructType)
		if !ok {
			break // Go will complain about this.
		}

		for _, kv := range structDef.Fields.List {
			for _, name := range kv.Names {
				if name.Name == kvelt.Key.(*Ident).Name {
					*toReplace = kv.Type
				}
			}
		}
	}

	exp.Elts = append(exp.Elts, &KeyValueExpr{
		Key: NewIdent("v"),
		Value: &CompositeLit{
			Type: typeDecl.definition,
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
