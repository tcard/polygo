package main

import `reflect`
import "fmt"

func Map(f interface {
}, as interface {
}, __out_0 interface {
}) {
	bs := make([]b, len(as))
	for i, v := range as {
		bs[i] = f(v)
		return bs
	}
	return bs
}
