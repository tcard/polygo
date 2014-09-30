package main

import "fmt"
import "reflect"

type OrderedMap struct {
	t0, t1	reflect.Type
	v	struct {
		m	map[interface {
		}]interface {
		}
		order	[]interface {
		}
	}
}

func (v OrderedMapPair) String() string {
	return fmt.Sprint(v.v)
}
func (v OrderedMap) String() string {
	return fmt.Sprint(v.v)
}

type OrderedMapPair struct {
	t0, t1	reflect.Type
	v	struct {
		key	interface {
		}
		value	interface {
		}
	}
}

func (m OrderedMap) Set(key interface {
}, value interface {
}) {
	__arg_t_0 := reflect.TypeOf(key)
	if __arg_t_0 != m.t0 {
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __arg_t_0, `k`, `key`, m.t0))
	}
	__arg_t_1 := reflect.TypeOf(value)
	if __arg_t_1 != m.t1 {
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __arg_t_1, `v`, `value`, m.t1))
	}
	m.v.m[key] = value
	m.v.order = append(m.v.order, key)
}
func (m OrderedMap) Get(key interface {
}, __out_0 interface {
}) bool {
	__arg_t_0 := reflect.TypeOf(key)
	if __arg_t_0 != m.t0 {
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __arg_t_0, `k`, `key`, m.t0))
	}
	__out_t_0 := reflect.TypeOf(__out_0)
	if __out_t_0.Kind() != reflect.Ptr || __out_t_0.Elem() != m.t1 {
		panic(fmt.Sprintf(`polygo: bad type %v for type parameter %v in argument %v, expected %v.`, __out_t_0, `v`, `__out_0`, m.t1))
	}
	val, ok := m.v.m[key]
	reflect.Indirect(reflect.ValueOf(__out_0)).Set(reflect.ValueOf(val))
	return ok
}
func main() {
	m := OrderedMap{t0: reflect.TypeOf(new(string)).Elem(), t1: reflect.TypeOf(new(int)).Elem(), v: struct {
		m	map[interface {
		}]interface {
		}
		order	[]interface {
		}
	}{m: map[interface {
	}]interface {
	}{"nene": 9999}}}
	m.Set("abc", 123)
	v, ok := m.Get("abc")
	fmt.Println(v, ok, m)
}
