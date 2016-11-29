// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

// For gccgo, use go:linkname to rename compiler-called functions to
// themselves, so that the compiler will export them.
//
//go:linkname interhash runtime.interhash
//go:linkname nilinterhash runtime.nilinterhash
//go:linkname interequal runtime.interequal
//go:linkname nilinterequal runtime.nilinterequal
//go:linkname efaceeq runtime.efaceeq
//go:linkname ifaceeq runtime.ifaceeq
//go:linkname ifacevaleq runtime.ifacevaleq
//go:linkname ifaceefaceeq runtime.ifaceefaceeq
//go:linkname efacevaleq runtime.efacevaleq
//go:linkname eqstring runtime.eqstring
//go:linkname cmpstring runtime.cmpstring

const (
	c0 = uintptr((8-sys.PtrSize)/4*2860486313 + (sys.PtrSize-4)/4*33054211828000289)
	c1 = uintptr((8-sys.PtrSize)/4*3267000013 + (sys.PtrSize-4)/4*23344194077549503)
)

func interhash(p unsafe.Pointer, h uintptr, size uintptr) uintptr {
	a := (*iface)(p)
	tab := a.tab
	if tab == nil {
		return h
	}
	t := *(**_type)(tab)
	fn := t.hashfn
	if fn == nil {
		panic(errorString("hash of unhashable type " + *t.string))
	}
	if isDirectIface(t) {
		return c1 * fn(unsafe.Pointer(&a.data), h^c0, t.size)
	} else {
		return c1 * fn(a.data, h^c0, t.size)
	}
}

func nilinterhash(p unsafe.Pointer, h uintptr, size uintptr) uintptr {
	a := (*eface)(p)
	t := a._type
	if t == nil {
		return h
	}
	fn := t.hashfn
	if fn == nil {
		panic(errorString("hash of unhashable type " + *t.string))
	}
	if isDirectIface(t) {
		return c1 * fn(unsafe.Pointer(&a.data), h^c0, t.size)
	} else {
		return c1 * fn(a.data, h^c0, t.size)
	}
}

func interequal(p, q unsafe.Pointer, size uintptr) bool {
	return ifaceeq(*(*iface)(p), *(*iface)(q))
}

func nilinterequal(p, q unsafe.Pointer, size uintptr) bool {
	return efaceeq(*(*eface)(p), *(*eface)(q))
}

func efaceeq(x, y eface) bool {
	t := x._type
	if !eqtype(t, y._type) {
		return false
	}
	if t == nil {
		return true
	}
	eq := t.equalfn
	if eq == nil {
		panic(errorString("comparing uncomparable type " + *t.string))
	}
	if isDirectIface(t) {
		return x.data == y.data
	}
	return eq(x.data, y.data, t.size)
}

func ifaceeq(x, y iface) bool {
	xtab := x.tab
	if xtab == nil && y.tab == nil {
		return true
	}
	if xtab == nil || y.tab == nil {
		return false
	}
	t := *(**_type)(xtab)
	if !eqtype(t, *(**_type)(y.tab)) {
		return false
	}
	eq := t.equalfn
	if eq == nil {
		panic(errorString("comparing uncomparable type " + *t.string))
	}
	if isDirectIface(t) {
		return x.data == y.data
	}
	return eq(x.data, y.data, t.size)
}

func ifacevaleq(x iface, t *_type, p unsafe.Pointer) bool {
	if x.tab == nil {
		return false
	}
	xt := *(**_type)(x.tab)
	if !eqtype(xt, t) {
		return false
	}
	eq := t.equalfn
	if eq == nil {
		panic(errorString("comparing uncomparable type " + *t.string))
	}
	if isDirectIface(t) {
		return x.data == p
	}
	return eq(x.data, p, t.size)
}

func ifaceefaceeq(x iface, y eface) bool {
	if x.tab == nil && y._type == nil {
		return true
	}
	if x.tab == nil || y._type == nil {
		return false
	}
	xt := *(**_type)(x.tab)
	if !eqtype(xt, y._type) {
		return false
	}
	eq := xt.equalfn
	if eq == nil {
		panic(errorString("comparing uncomparable type " + *xt.string))
	}
	if isDirectIface(xt) {
		return x.data == y.data
	}
	return eq(x.data, y.data, xt.size)
}

func efacevaleq(x eface, t *_type, p unsafe.Pointer) bool {
	if x._type == nil {
		return false
	}
	if !eqtype(x._type, t) {
		return false
	}
	eq := t.equalfn
	if eq == nil {
		panic(errorString("comparing uncomparable type " + *t.string))
	}
	if isDirectIface(t) {
		return x.data == p
	}
	return eq(x.data, p, t.size)
}

func eqstring(x, y string) bool {
	a := stringStructOf(&x)
	b := stringStructOf(&y)
	if a.len != b.len {
		return false
	}
	return memcmp(unsafe.Pointer(a.str), unsafe.Pointer(b.str), uintptr(a.len)) == 0
}

func cmpstring(x, y string) int {
	a := stringStructOf(&x)
	b := stringStructOf(&y)
	l := a.len
	if l > b.len {
		l = b.len
	}
	i := memcmp(unsafe.Pointer(a.str), unsafe.Pointer(b.str), uintptr(l))
	if i != 0 {
		return int(i)
	}
	if a.len < b.len {
		return -1
	} else if a.len > b.len {
		return 1
	}
	return 0
}

// Force the creation of function descriptors for equality and hash
// functions.  These will be referenced directly by the compiler.
var _ = interhash
var _ = interequal
var _ = nilinterhash
var _ = nilinterequal
