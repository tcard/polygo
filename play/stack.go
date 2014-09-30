package main

import "reflect"
import "fmt"

func (v Stack) String() string {
	return fmt.Sprint(v.v)
}
func (v test) String() string {
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
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __arg_t_0, `a`, `x`, s.t0))
	}
	s.v = append(s.v, x)
}
func (s *Stack) Pop(popped interface {
}) (jarl int) {
	__out_t_0 := reflect.TypeOf(popped)
	if __out_t_0.Kind() != reflect.Ptr || __out_t_0.Elem() != s.t0 {
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __out_t_0, `a`, `popped`, s.t0))
	}
	x := s.v[len(s.v)-1]
	s.v = s.v[:len(s.v)-1]
	reflect.Indirect(reflect.ValueOf(popped)).Set(reflect.ValueOf(x))
	return 123
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
	b.Push("lalala")
	b.Push("lelele")
	fmt.Println(a, b.v)
	fmt.Println(func() (string, int) {
		var popped string
		jarl := b.Pop(&popped)
		return popped, jarl
	}())
	fmt.Println(b.v)
}
