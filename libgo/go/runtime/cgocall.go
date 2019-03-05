// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Cgo call and callback support.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

// Functions called by cgo-generated code.
//go:linkname cgoCheckPointer runtime.cgoCheckPointer
//go:linkname cgoCheckResult runtime.cgoCheckResult

// Pointer checking for cgo code.

// We want to detect all cases where a program that does not use
// unsafe makes a cgo call passing a Go pointer to memory that
// contains a Go pointer. Here a Go pointer is defined as a pointer
// to memory allocated by the Go runtime. Programs that use unsafe
// can evade this restriction easily, so we don't try to catch them.
// The cgo program will rewrite all possibly bad pointer arguments to
// call cgoCheckPointer, where we can catch cases of a Go pointer
// pointing to a Go pointer.

// Complicating matters, taking the address of a slice or array
// element permits the C program to access all elements of the slice
// or array. In that case we will see a pointer to a single element,
// but we need to check the entire data structure.

// The cgoCheckPointer call takes additional arguments indicating that
// it was called on an address expression. An additional argument of
// true means that it only needs to check a single element. An
// additional argument of a slice or array means that it needs to
// check the entire slice/array, but nothing else. Otherwise, the
// pointer could be anything, and we check the entire heap object,
// which is conservative but safe.

// When and if we implement a moving garbage collector,
// cgoCheckPointer will pin the pointer for the duration of the cgo
// call.  (This is necessary but not sufficient; the cgo program will
// also have to change to pin Go pointers that cannot point to Go
// pointers.)

// cgoCheckPointer checks if the argument contains a Go pointer that
// points to a Go pointer, and panics if it does.
func cgoCheckPointer(ptr interface{}, args ...interface{}) {
	if debug.cgocheck == 0 {
		return
	}

	ep := (*eface)(unsafe.Pointer(&ptr))
	t := ep._type

	top := true
	if len(args) > 0 && (t.kind&kindMask == kindPtr || t.kind&kindMask == kindUnsafePointer) {
		p := ep.data
		if t.kind&kindDirectIface == 0 {
			p = *(*unsafe.Pointer)(p)
		}
		if !cgoIsGoPointer(p) {
			return
		}
		aep := (*eface)(unsafe.Pointer(&args[0]))
		switch aep._type.kind & kindMask {
		case kindBool:
			if t.kind&kindMask == kindUnsafePointer {
				// We don't know the type of the element.
				break
			}
			pt := (*ptrtype)(unsafe.Pointer(t))
			cgoCheckArg(pt.elem, p, true, false, cgoCheckPointerFail)
			return
		case kindSlice:
			// Check the slice rather than the pointer.
			ep = aep
			t = ep._type
		case kindArray:
			// Check the array rather than the pointer.
			// Pass top as false since we have a pointer
			// to the array.
			ep = aep
			t = ep._type
			top = false
		default:
			throw("can't happen")
		}
	}

	cgoCheckArg(t, ep.data, t.kind&kindDirectIface == 0, top, cgoCheckPointerFail)
}

const cgoCheckPointerFail = "cgo argument has Go pointer to Go pointer"
const cgoResultFail = "cgo result has Go pointer"

// cgoCheckArg is the real work of cgoCheckPointer. The argument p
// is either a pointer to the value (of type t), or the value itself,
// depending on indir. The top parameter is whether we are at the top
// level, where Go pointers are allowed.
func cgoCheckArg(t *_type, p unsafe.Pointer, indir, top bool, msg string) {
	if t.kind&kindNoPointers != 0 {
		// If the type has no pointers there is nothing to do.
		return
	}

	switch t.kind & kindMask {
	default:
		throw("can't happen")
	case kindArray:
		at := (*arraytype)(unsafe.Pointer(t))
		if !indir {
			if at.len != 1 {
				throw("can't happen")
			}
			cgoCheckArg(at.elem, p, at.elem.kind&kindDirectIface == 0, top, msg)
			return
		}
		for i := uintptr(0); i < at.len; i++ {
			cgoCheckArg(at.elem, p, true, top, msg)
			p = add(p, at.elem.size)
		}
	case kindChan, kindMap:
		// These types contain internal pointers that will
		// always be allocated in the Go heap. It's never OK
		// to pass them to C.
		panic(errorString(msg))
	case kindFunc:
		if indir {
			p = *(*unsafe.Pointer)(p)
		}
		if !cgoIsGoPointer(p) {
			return
		}
		panic(errorString(msg))
	case kindInterface:
		it := *(**_type)(p)
		if it == nil {
			return
		}
		// A type known at compile time is OK since it's
		// constant. A type not known at compile time will be
		// in the heap and will not be OK.
		if inheap(uintptr(unsafe.Pointer(it))) {
			panic(errorString(msg))
		}
		p = *(*unsafe.Pointer)(add(p, sys.PtrSize))
		if !cgoIsGoPointer(p) {
			return
		}
		if !top {
			panic(errorString(msg))
		}
		cgoCheckArg(it, p, it.kind&kindDirectIface == 0, false, msg)
	case kindSlice:
		st := (*slicetype)(unsafe.Pointer(t))
		s := (*slice)(p)
		p = s.array
		if !cgoIsGoPointer(p) {
			return
		}
		if !top {
			panic(errorString(msg))
		}
		if st.elem.kind&kindNoPointers != 0 {
			return
		}
		for i := 0; i < s.cap; i++ {
			cgoCheckArg(st.elem, p, true, false, msg)
			p = add(p, st.elem.size)
		}
	case kindString:
		ss := (*stringStruct)(p)
		if !cgoIsGoPointer(ss.str) {
			return
		}
		if !top {
			panic(errorString(msg))
		}
	case kindStruct:
		st := (*structtype)(unsafe.Pointer(t))
		if !indir {
			if len(st.fields) != 1 {
				throw("can't happen")
			}
			cgoCheckArg(st.fields[0].typ, p, st.fields[0].typ.kind&kindDirectIface == 0, top, msg)
			return
		}
		for _, f := range st.fields {
			cgoCheckArg(f.typ, add(p, f.offset()), true, top, msg)
		}
	case kindPtr, kindUnsafePointer:
		if indir {
			p = *(*unsafe.Pointer)(p)
		}

		if !cgoIsGoPointer(p) {
			return
		}
		if !top {
			panic(errorString(msg))
		}

		cgoCheckUnknownPointer(p, msg)
	}
}

// cgoCheckUnknownPointer is called for an arbitrary pointer into Go
// memory. It checks whether that Go memory contains any other
// pointer into Go memory. If it does, we panic.
// The return values are unused but useful to see in panic tracebacks.
func cgoCheckUnknownPointer(p unsafe.Pointer, msg string) (base, i uintptr) {
	if inheap(uintptr(p)) {
		b, span, _ := findObject(uintptr(p), 0, 0, false)
		base = b
		if base == 0 {
			return
		}
		hbits := heapBitsForAddr(base)
		n := span.elemsize
		for i = uintptr(0); i < n; i += sys.PtrSize {
			if i != 1*sys.PtrSize && !hbits.morePointers() {
				// No more possible pointers.
				break
			}
			if hbits.isPointer() && cgoIsGoPointer(*(*unsafe.Pointer)(unsafe.Pointer(base + i))) {
				panic(errorString(msg))
			}
			hbits = hbits.next()
		}

		return
	}

	lo := 0
	hi := len(gcRootsIndex)
	for lo < hi {
		m := lo + (hi-lo)/2
		pr := gcRootsIndex[m]
		addr := uintptr(pr.decl)
		if cgoInRange(p, addr, addr+pr.size) {
			cgoCheckBits(pr.decl, pr.gcdata, 0, pr.ptrdata)
			return
		}
		if uintptr(p) < addr {
			hi = m
		} else {
			lo = m + 1
		}
	}

	return
}

// cgoIsGoPointer reports whether the pointer is a Go pointer--a
// pointer to Go memory. We only care about Go memory that might
// contain pointers.
//go:nosplit
//go:nowritebarrierrec
func cgoIsGoPointer(p unsafe.Pointer) bool {
	if p == nil {
		return false
	}

	if inHeapOrStack(uintptr(p)) {
		return true
	}

	roots := gcRoots
	for roots != nil {
		for i := 0; i < roots.count; i++ {
			pr := roots.roots[i]
			addr := uintptr(pr.decl)
			if cgoInRange(p, addr, addr+pr.size) {
				return true
			}
		}
		roots = roots.next
	}

	return false
}

// cgoInRange reports whether p is between start and end.
//go:nosplit
//go:nowritebarrierrec
func cgoInRange(p unsafe.Pointer, start, end uintptr) bool {
	return start <= uintptr(p) && uintptr(p) < end
}

// cgoCheckResult is called to check the result parameter of an
// exported Go function. It panics if the result is or contains a Go
// pointer.
func cgoCheckResult(val interface{}) {
	if debug.cgocheck == 0 {
		return
	}

	ep := (*eface)(unsafe.Pointer(&val))
	t := ep._type
	cgoCheckArg(t, ep.data, t.kind&kindDirectIface == 0, false, cgoResultFail)
}
