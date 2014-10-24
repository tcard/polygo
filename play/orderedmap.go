package main

import `fmt`
import `reflect`

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
	m.v.m[key] = value
	m.v.order = append(m.v.order, key)
}
func (m OrderedMap) Get(key interface {
}, __out_0 interface {
}) bool {
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
	v, ok := func() (int, bool) {
		var out0 int
		out1 := m.Get("abc", &out0)
		return out0, out1
	}()
	fmt.Println(v, ok, m.v)
	fmt.Println(func() (int, bool) {
		var out0 int
		out1 := m.Get("abc", &out0)
		return out0, out1
	}())
}
