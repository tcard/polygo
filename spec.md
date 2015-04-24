# polygo

Polygo extends Go by:

* Allowing defined types to leave parts of their definition not fully set, but bound to _type parameters_. Instances of a _type-parameterized type_ must provide the parameters needed.

* Allowing functions and methods to take not only value parameters (arguments), but also type parameters, to which both arguments and return values can be bound.

## Extensions to the Go syntax

### Type parameters list

	TypeParams = "<" identifier { "," identifier } ">" .

### Parameterized type declarations

	TypeSpec = [ TypeParams ] identifier Type .

### Parameterized functions

	FunctionDecl = "func" [ TypeParams ] FunctionName ( Function | Signature ) .

### Parameterized methods
	MethodDecl   = "func" Receiver [ TypeParams ] MethodName ( Function | Signature ) .

### Bound types

	Type = [ TypeParams ] TypeName | TypeLit | "(" Type ")" .

Note that only named types can be type-parameterized.

## Binding of types to type parameters

A type T is bound to a type parameter U when substituting the type parameter U by a type further specializes T.

A type is fully specialized when all the type parameters it is bound to are substituted by fully specialized types.

A value can only have a full specialized type.

Here, the type `Stack` is bound to a type parameter `T`:

```go
type <T>Stack []T
```

In this function, the type of the argument `haystack` is bound to the type parameters `k` and `v`; and the type of the first return value is bound to the type parameter `v`.

```go
func <k, v>Lookup(haystack map[k]v, needle k) (v, bool)
```

In this method, the type of the first return value is bound to the type parameter `T`.

```go
type <T>Stack []T

func (s <T>Stack) Pop() (T, bool)
```

## Specializing types

Here, the type `IntStack` is a specialization of type `<int>Stack`:

```go
type IntStack <int>Stack

var intStack = IntStack{1, 2, 3}
```

Another way of specializing a type is by constructing a value either with a literal, `make` or `new`, or a type conversion of a parameterized type. The type of the value will be a specialization of the parameterized type:

```go
var intStack = <int>Stack{1, 2, 3}
var byteStack = make(<byte>Stack)
var stringStack <string>Stack([]string{"a", "b", "c"})
```

Calling a type-parameterized function specializes the types in the argument list, the return values and the body of the function.

```go
func <a, b>Map(f func(a) b, items []a) []b {
	mapped := make([]b, len(items))
	for k, v := range items {
		mapped[k] = f(v)
	}
	return mapped
}

<int, string>Map(strconv.Itoa, []int{1, 2, 3}) // Returns []string
```

Constructing a type-parameterized type with methods specializes the types bound to the type parameters of the type in the argument lists, the return values and the bodies of its methods.

```go
type <T>Stack []T

func (s <U>Stack) Pop() (U, bool) {
	if len(s) == 0 {
		return *new(U), false
	}
	popped := s[len(s) - 1]
	s = s[:len(s) - 1]
	return popped, true
}

var intStack = <int>Stack{1, 2, 3}
p, ok := intStack.Pop() // p has type int.
```

# Binding semantics

## Bound interface types

A value can be converted to a bound interface type when, once specialized, the conversion complies with [the spec for non-type-parameterized interface types](http://golang.org/ref/spec#Interface_types).

```go
type <T>Stack interface{
	Pop() (T, bool)
	Push(x T)
}

type <T>SliceStack []T

func (s <T>SliceStack) Pop() (T, bool)
func (s <T>SliceStack) Push(x T)

type IntSliceStack []int

func (s IntSliceStack) Pop() (int, bool)
func (s IntSliceStack) Push(x int)

var st <int>Stack = <int>SliceStack{1, 2, 3}
var st2 <int>Stack = IntSliceStack{1, 2, 3}
// Valid for the same reason that this is valid:
var st3 interface{
	Pop() (int, bool)
	Push(x int)
} = <int>SliceStack{1, 2, 3}
```

