// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"internal/cpu"
	"runtime/internal/sys"
	"unsafe"
)

// For gccgo, use go:linkname to rename compiler-called functions to
// themselves, so that the compiler will export them.
//
//go:linkname memhash0 runtime.memhash0
//go:linkname memhash8 runtime.memhash8
//go:linkname memhash16 runtime.memhash16
//go:linkname memhash32 runtime.memhash32
//go:linkname memhash64 runtime.memhash64
//go:linkname memhash128 runtime.memhash128
//go:linkname strhash runtime.strhash
//go:linkname f32hash runtime.f32hash
//go:linkname f64hash runtime.f64hash
//go:linkname c64hash runtime.c64hash
//go:linkname c128hash runtime.c128hash
//go:linkname interhash runtime.interhash
//go:linkname nilinterhash runtime.nilinterhash
//go:linkname memequal0 runtime.memequal0
//go:linkname memequal8 runtime.memequal8
//go:linkname memequal16 runtime.memequal16
//go:linkname memequal32 runtime.memequal32
//go:linkname memequal64 runtime.memequal64
//go:linkname memequal128 runtime.memequal128
//go:linkname strequal runtime.strequal
//go:linkname f32equal runtime.f32equal
//go:linkname f64equal runtime.f64equal
//go:linkname c64equal runtime.c64equal
//go:linkname c128equal runtime.c128equal
//go:linkname interequal runtime.interequal
//go:linkname nilinterequal runtime.nilinterequal
//go:linkname efaceeq runtime.efaceeq
//go:linkname ifaceeq runtime.ifaceeq
//go:linkname ifacevaleq runtime.ifacevaleq
//go:linkname ifaceefaceeq runtime.ifaceefaceeq
//go:linkname efacevaleq runtime.efacevaleq
//go:linkname eqstring runtime.eqstring
//go:linkname cmpstring runtime.cmpstring
//
// Temporary to be called from C code.
//go:linkname alginit runtime.alginit

const (
	c0 = uintptr((8-sys.PtrSize)/4*2860486313 + (sys.PtrSize-4)/4*33054211828000289)
	c1 = uintptr((8-sys.PtrSize)/4*3267000013 + (sys.PtrSize-4)/4*23344194077549503)
)

func memhash0(p unsafe.Pointer, h uintptr) uintptr {
	return h
}

func memhash8(p unsafe.Pointer, h uintptr) uintptr {
	return memhash(p, h, 1)
}

func memhash16(p unsafe.Pointer, h uintptr) uintptr {
	return memhash(p, h, 2)
}

func memhash128(p unsafe.Pointer, h uintptr) uintptr {
	return memhash(p, h, 16)
}

var useAeshash bool

// in C code
func aeshashbody(p unsafe.Pointer, h, s uintptr, sched []byte) uintptr

func aeshash(p unsafe.Pointer, h, s uintptr) uintptr {
	return aeshashbody(p, h, s, aeskeysched[:])
}

func aeshashstr(p unsafe.Pointer, h uintptr) uintptr {
	ps := (*stringStruct)(p)
	return aeshashbody(unsafe.Pointer(ps.str), h, uintptr(ps.len), aeskeysched[:])
}

func strhash(a unsafe.Pointer, h uintptr) uintptr {
	x := (*stringStruct)(a)
	return memhash(x.str, h, uintptr(x.len))
}

// NOTE: Because NaN != NaN, a map can contain any
// number of (mostly useless) entries keyed with NaNs.
// To avoid long hash chains, we assign a random number
// as the hash value for a NaN.

func f32hash(p unsafe.Pointer, h uintptr) uintptr {
	f := *(*float32)(p)
	switch {
	case f == 0:
		return c1 * (c0 ^ h) // +0, -0
	case f != f:
		return c1 * (c0 ^ h ^ uintptr(fastrand())) // any kind of NaN
	default:
		return memhash(p, h, 4)
	}
}

func f64hash(p unsafe.Pointer, h uintptr) uintptr {
	f := *(*float64)(p)
	switch {
	case f == 0:
		return c1 * (c0 ^ h) // +0, -0
	case f != f:
		return c1 * (c0 ^ h ^ uintptr(fastrand())) // any kind of NaN
	default:
		return memhash(p, h, 8)
	}
}

func c64hash(p unsafe.Pointer, h uintptr) uintptr {
	x := (*[2]float32)(p)
	return f32hash(unsafe.Pointer(&x[1]), f32hash(unsafe.Pointer(&x[0]), h))
}

func c128hash(p unsafe.Pointer, h uintptr) uintptr {
	x := (*[2]float64)(p)
	return f64hash(unsafe.Pointer(&x[1]), f64hash(unsafe.Pointer(&x[0]), h))
}

func interhash(p unsafe.Pointer, h uintptr) uintptr {
	a := (*iface)(p)
	tab := a.tab
	if tab == nil {
		return h
	}
	t := *(**_type)(tab)
	fn := t.hashfn
	if fn == nil {
		panic(errorString("hash of unhashable type " + t.string()))
	}
	if isDirectIface(t) {
		return c1 * fn(unsafe.Pointer(&a.data), h^c0)
	} else {
		return c1 * fn(a.data, h^c0)
	}
}

func nilinterhash(p unsafe.Pointer, h uintptr) uintptr {
	a := (*eface)(p)
	t := a._type
	if t == nil {
		return h
	}
	fn := t.hashfn
	if fn == nil {
		panic(errorString("hash of unhashable type " + t.string()))
	}
	if isDirectIface(t) {
		return c1 * fn(unsafe.Pointer(&a.data), h^c0)
	} else {
		return c1 * fn(a.data, h^c0)
	}
}

func memequal0(p, q unsafe.Pointer) bool {
	return true
}
func memequal8(p, q unsafe.Pointer) bool {
	return *(*int8)(p) == *(*int8)(q)
}
func memequal16(p, q unsafe.Pointer) bool {
	return *(*int16)(p) == *(*int16)(q)
}
func memequal32(p, q unsafe.Pointer) bool {
	return *(*int32)(p) == *(*int32)(q)
}
func memequal64(p, q unsafe.Pointer) bool {
	return *(*int64)(p) == *(*int64)(q)
}
func memequal128(p, q unsafe.Pointer) bool {
	return *(*[2]int64)(p) == *(*[2]int64)(q)
}
func f32equal(p, q unsafe.Pointer) bool {
	return *(*float32)(p) == *(*float32)(q)
}
func f64equal(p, q unsafe.Pointer) bool {
	return *(*float64)(p) == *(*float64)(q)
}
func c64equal(p, q unsafe.Pointer) bool {
	return *(*complex64)(p) == *(*complex64)(q)
}
func c128equal(p, q unsafe.Pointer) bool {
	return *(*complex128)(p) == *(*complex128)(q)
}
func strequal(p, q unsafe.Pointer) bool {
	return *(*string)(p) == *(*string)(q)
}
func interequal(p, q unsafe.Pointer) bool {
	return ifaceeq(*(*iface)(p), *(*iface)(q))
}
func nilinterequal(p, q unsafe.Pointer) bool {
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
		panic(errorString("comparing uncomparable type " + t.string()))
	}
	if isDirectIface(t) {
		return x.data == y.data
	}
	return eq(x.data, y.data)
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
		panic(errorString("comparing uncomparable type " + t.string()))
	}
	if isDirectIface(t) {
		return x.data == y.data
	}
	return eq(x.data, y.data)
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
		panic(errorString("comparing uncomparable type " + t.string()))
	}
	if isDirectIface(t) {
		return x.data == p
	}
	return eq(x.data, p)
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
		panic(errorString("comparing uncomparable type " + xt.string()))
	}
	if isDirectIface(xt) {
		return x.data == y.data
	}
	return eq(x.data, y.data)
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
		panic(errorString("comparing uncomparable type " + t.string()))
	}
	if isDirectIface(t) {
		return x.data == p
	}
	return eq(x.data, p)
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

// For the unsafe.Pointer type descriptor in libgo/runtime/go-unsafe-pointer.c.

func pointerhash(p unsafe.Pointer, h uintptr) uintptr {
	return memhash(p, h, unsafe.Sizeof(unsafe.Pointer))
}

func pointerequal(p, q unsafe.Pointer) bool {
	return *(*unsafe.Pointer)(p) == *(*unsafe.Pointer)(q)
}

// Force the creation of function descriptors for equality and hash
// functions.  These will be referenced directly by the compiler.
var _ = memhash
var _ = memhash0
var _ = memhash8
var _ = memhash16
var _ = memhash32
var _ = memhash64
var _ = memhash128
var _ = strhash
var _ = f32hash
var _ = f64hash
var _ = c64hash
var _ = c128hash
var _ = interhash
var _ = nilinterhash
var _ = memequal0
var _ = memequal8
var _ = memequal16
var _ = memequal32
var _ = memequal64
var _ = memequal128
var _ = f32equal
var _ = f64equal
var _ = c64equal
var _ = c128equal
var _ = strequal
var _ = interequal
var _ = nilinterequal
var _ = pointerhash
var _ = pointerequal

// Testing adapters for hash quality tests (see hash_test.go)
func stringHash(s string, seed uintptr) uintptr {
	return strhash(noescape(unsafe.Pointer(&s)), seed)
}

func bytesHash(b []byte, seed uintptr) uintptr {
	s := (*slice)(unsafe.Pointer(&b))
	return memhash(s.array, seed, uintptr(s.len))
}

func int32Hash(i uint32, seed uintptr) uintptr {
	return memhash32(noescape(unsafe.Pointer(&i)), seed)
}

func int64Hash(i uint64, seed uintptr) uintptr {
	return memhash64(noescape(unsafe.Pointer(&i)), seed)
}

func efaceHash(i interface{}, seed uintptr) uintptr {
	return nilinterhash(noescape(unsafe.Pointer(&i)), seed)
}

func ifaceHash(i interface {
	F()
}, seed uintptr) uintptr {
	return interhash(noescape(unsafe.Pointer(&i)), seed)
}

const hashRandomBytes = sys.PtrSize / 4 * 64

// used in asm_{386,amd64,arm64}.s to seed the hash function
var aeskeysched [hashRandomBytes]byte

// used in hash{32,64}.go to seed the hash function
var hashkey [4]uintptr

func alginit() {
	// Install AES hash algorithms if the instructions needed are present.
	if (GOARCH == "386" || GOARCH == "amd64") &&
		GOOS != "nacl" &&
		support_aes &&
		cpu.X86.HasAES && // AESENC
		cpu.X86.HasSSSE3 && // PSHUFB
		cpu.X86.HasSSE41 { // PINSR{D,Q}
		initAlgAES()
		return
	}
	if GOARCH == "arm64" && cpu.ARM64.HasAES {
		initAlgAES()
		return
	}
	getRandomData((*[len(hashkey) * sys.PtrSize]byte)(unsafe.Pointer(&hashkey))[:])
	hashkey[0] |= 1 // make sure these numbers are odd
	hashkey[1] |= 1
	hashkey[2] |= 1
	hashkey[3] |= 1
}

func initAlgAES() {
	useAeshash = true
	// Initialize with random data so hash collisions will be hard to engineer.
	getRandomData(aeskeysched[:])
}
