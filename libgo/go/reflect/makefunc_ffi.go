// Copyright 2014 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package reflect

import (
	"internal/goarch"
	"unsafe"
)

// The makeFuncFFI function, written in C, fills in an FFI closure.
// It arranges for ffiCall to be invoked directly from FFI.
func makeFuncFFI(cif unsafe.Pointer, impl unsafe.Pointer)

// The makeCIF function, implemented in the runtime package, allocates a CIF.
func makeCIF(ft *funcType) unsafe.Pointer

// Export ffiCallbackGo so that C code in makefunc_ffi_c.c can call it.
//go:linkname ffiCallbackGo

// ffiCallbackGo implements the Go side of the libffi callback.
//
// The call chain arriving here looks like
//   some_go_caller
//   ->some_ffi_internals
//     ->ffi_callback (in C)
//       ->ffiCallbackGo
//
// The ffi_callback handles __go_makefunc_can_recover, and
// then passes off the data as received from ffi here.
func ffiCallbackGo(results unsafe.Pointer, params unsafe.Pointer, impl *makeFuncImpl, wordsize int32, bigEndian bool) {
	ftyp := impl.typ
	in := make([]Value, 0, len(ftyp.in))
	ap := params
	for _, rt := range ftyp.in {
		p := unsafe_New(rt)
		typedmemmove(rt, p, *(*unsafe.Pointer)(ap))
		v := Value{rt, p, flag(rt.Kind()) | flagIndir}
		in = append(in, v)
		ap = (unsafe.Pointer)(uintptr(ap) + goarch.PtrSize)
	}

	out := impl.call(in)

	checkValue := func(v Value, typ *rtype, addr unsafe.Pointer) {
		if v.flag&flagRO != 0 {
			panic("reflect: function created by MakeFunc using " + funcName(impl.fn) +
				" returned value obtained from unexported field")
		}

		// Convert v to type typ if v is assignable to a variable
		// of type t in the language spec.
		// See issue 28761.
		v = v.assignTo("reflect.MakeFunc", typ, addr)
	}

	// In libffi a single integer return value is always promoted
	// to a full word. This only matters for integers whose size
	// is less than the size of a full word. There is similar code
	// in libgo/runtime/go-reflect-call.c.
	if len(ftyp.out) == 1 {
		typ := ftyp.out[0]
		switch typ.Kind() {
		case Bool, Int8, Int16, Int32, Uint8, Uint16, Uint32:
			v := out[0]
			checkValue(v, typ, nil)

			if bigEndian {
				results = unsafe.Pointer(uintptr(results) + uintptr(wordsize) - typ.size)
			}

			memmove(results, v.ptr, typ.size)
			return
		}
	}

	off := uintptr(0)
	for i, typ := range ftyp.out {
		v := out[i]

		off = align(off, uintptr(typ.fieldAlign))
		addr := unsafe.Pointer(uintptr(results) + off)

		checkValue(v, typ, addr)

		if v.flag&flagIndir == 0 && (v.kind() == Ptr || v.kind() == UnsafePointer) {
			*(*unsafe.Pointer)(addr) = v.ptr
		} else {
			typedmemmove(typ, addr, v.ptr)
		}
		off += typ.size
	}
}
