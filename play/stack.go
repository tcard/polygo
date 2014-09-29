package main

import "reflect"
import "fmt"

func (v test) String() string {
	return fmt.Sprint(v.v)
}
func (v Stack) String() string {
	return fmt.Sprint(v.v)
}

type Stack struct {
	t0	reflect.Type
	v	[]interface {
	}
}

func (s *Stack) Push(x interface {
}) {
	__arg_t_0 := reflect.TypeOf(x)
	if __arg_t_0 != s.t0 {
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __arg_t_0, `T`, `x`, s.t0))
	}
	s.v = append(s.v, x)
}
func (s *Stack) Pop(out interface {
}) {
	__out_t_0 := reflect.TypeOf(out)
	if __out_t_0.Kind() != reflect.Ptr || __out_t_0.Elem() != s.t0 {
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __out_t_0, `T`, `out`, s.t0))
	}
	x := s.v[len(s.v)-1]
	reflect.Indirect(reflect.ValueOf(out)).Set(reflect.ValueOf(x))
	s.v = s.v[:len(s.v)-1]
}

type test struct {
	t0	reflect.Type
	v	interface {
	}
}

var s Stack = Stack{t0: reflect.TypeOf(new(int)).Elem(), v: []interface {
}{}}

func main() {
	a := Stack{t0: reflect.TypeOf(new(int)).Elem(), v: []interface {
	}{123, 456}}
	var b Stack = Stack{t0: reflect.TypeOf(new(string)).Elem(), v: []interface {
	}{}}
	b.v.Push("lalala")
	b.v.Push("lelele")
	fmt.Println(a, b.v)
	fmt.Println(func() string {
		var out0 string
		b.Pop(&out0)
		return out0
	}())
	fmt.Println(b.v)
}
