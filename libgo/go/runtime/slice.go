// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"unsafe"
)

// For gccgo, use go:linkname to rename compiler-called functions to
// themselves, so that the compiler will export them.
//
//go:linkname makeslice runtime.makeslice
//go:linkname makeslice64 runtime.makeslice64
//go:linkname growslice runtime.growslice
//go:linkname slicecopy runtime.slicecopy
//go:linkname slicestringcopy runtime.slicestringcopy

type slice struct {
	array unsafe.Pointer
	len   int
	cap   int
}

// maxElems is a lookup table containing the maximum capacity for a slice.
// The index is the size of the slice element.
var maxElems = [...]uintptr{
	^uintptr(0),
	_MaxMem / 1, _MaxMem / 2, _MaxMem / 3, _MaxMem / 4,
	_MaxMem / 5, _MaxMem / 6, _MaxMem / 7, _MaxMem / 8,
	_MaxMem / 9, _MaxMem / 10, _MaxMem / 11, _MaxMem / 12,
	_MaxMem / 13, _MaxMem / 14, _MaxMem / 15, _MaxMem / 16,
	_MaxMem / 17, _MaxMem / 18, _MaxMem / 19, _MaxMem / 20,
	_MaxMem / 21, _MaxMem / 22, _MaxMem / 23, _MaxMem / 24,
	_MaxMem / 25, _MaxMem / 26, _MaxMem / 27, _MaxMem / 28,
	_MaxMem / 29, _MaxMem / 30, _MaxMem / 31, _MaxMem / 32,
}

// maxSliceCap returns the maximum capacity for a slice.
func maxSliceCap(elemsize uintptr) uintptr {
	if elemsize < uintptr(len(maxElems)) {
		return maxElems[elemsize]
	}
	return _MaxMem / elemsize
}

func makeslice(et *_type, len, cap int) slice {
	// NOTE: The len > maxElements check here is not strictly necessary,
	// but it produces a 'len out of range' error instead of a 'cap out of range' error
	// when someone does make([]T, bignumber). 'cap out of range' is true too,
	// but since the cap is only being supplied implicitly, saying len is clearer.
	// See issue 4085.
	maxElements := maxSliceCap(et.size)
	if len < 0 || uintptr(len) > maxElements {
		panic(errorString("makeslice: len out of range"))
	}

	if cap < len || uintptr(cap) > maxElements {
		panic(errorString("makeslice: cap out of range"))
	}

	// gccgo's current garbage collector requires using newarray,
	// not mallocgc here.  This can change back to mallocgc when
	// we port the garbage collector.
	p := newarray(et, cap)
	return slice{p, len, cap}
}

func makeslice64(et *_type, len64, cap64 int64) slice {
	len := int(len64)
	if int64(len) != len64 {
		panic(errorString("makeslice: len out of range"))
	}

	cap := int(cap64)
	if int64(cap) != cap64 {
		panic(errorString("makeslice: cap out of range"))
	}

	return makeslice(et, len, cap)
}

// growslice handles slice growth during append.
// It is passed the slice element type, the old slice, and the desired new minimum capacity,
// and it returns a new slice with at least that capacity, with the old data
// copied into it.
// The new slice's length is set to the requested capacity.
func growslice(et *_type, old slice, cap int) slice {
	if raceenabled {
		callerpc := getcallerpc(unsafe.Pointer(&et))
		racereadrangepc(old.array, uintptr(old.len*int(et.size)), callerpc, funcPC(growslice))
	}
	if msanenabled {
		msanread(old.array, uintptr(old.len*int(et.size)))
	}

	if et.size == 0 {
		if cap < old.cap {
			panic(errorString("growslice: cap out of range"))
		}
		// append should not create a slice with nil pointer but non-zero len.
		// We assume that append doesn't need to preserve old.array in this case.
		return slice{unsafe.Pointer(&zerobase), cap, cap}
	}

	newcap := old.cap
	doublecap := newcap + newcap
	if cap > doublecap {
		newcap = cap
	} else {
		if old.len < 1024 {
			newcap = doublecap
		} else {
			for newcap < cap {
				newcap += newcap / 4
			}
		}
	}

	var lenmem, newlenmem, capmem uintptr
	const ptrSize = unsafe.Sizeof((*byte)(nil))
	switch et.size {
	case 1:
		lenmem = uintptr(old.len)
		newlenmem = uintptr(cap)
		capmem = roundupsize(uintptr(newcap))
		newcap = int(capmem)
	case ptrSize:
		lenmem = uintptr(old.len) * ptrSize
		newlenmem = uintptr(cap) * ptrSize
		capmem = roundupsize(uintptr(newcap) * ptrSize)
		newcap = int(capmem / ptrSize)
	default:
		lenmem = uintptr(old.len) * et.size
		newlenmem = uintptr(cap) * et.size
		capmem = roundupsize(uintptr(newcap) * et.size)
		newcap = int(capmem / et.size)
	}

	if cap < old.cap || uintptr(newcap) > maxSliceCap(et.size) {
		panic(errorString("growslice: cap out of range"))
	}

	var p unsafe.Pointer
	if et.kind&kindNoPointers != 0 {
		// gccgo's current GC requires newarray, not mallocgc.
		p = newarray(et, newcap)
		memmove(p, old.array, lenmem)
		// The call to memclr is not needed for gccgo since
		// the newarray function will zero the memory.
		// Calling memclr is also wrong since we allocated
		// newcap*et.size bytes, which is not the same as capmem.
		// The append() that calls growslice is going to overwrite from old.len to cap (which will be the new length).
		// Only clear the part that will not be overwritten.
		// memclrNoHeapPointers(add(p, newlenmem), capmem-newlenmem)
		_ = newlenmem
	} else {
		// Note: can't use rawmem (which avoids zeroing of memory), because then GC can scan uninitialized memory.
		// gccgo's current GC requires newarray, not mallocgc.
		p = newarray(et, newcap)
		if !writeBarrier.enabled {
			memmove(p, old.array, lenmem)
		} else {
			for i := uintptr(0); i < lenmem; i += et.size {
				typedmemmove(et, add(p, i), add(old.array, i))
			}
		}
	}

	return slice{p, cap, newcap}
}

func slicecopy(to, fm slice, width uintptr) int {
	if fm.len == 0 || to.len == 0 {
		return 0
	}

	n := fm.len
	if to.len < n {
		n = to.len
	}

	if width == 0 {
		return n
	}

	if raceenabled {
		callerpc := getcallerpc(unsafe.Pointer(&to))
		pc := funcPC(slicecopy)
		racewriterangepc(to.array, uintptr(n*int(width)), callerpc, pc)
		racereadrangepc(fm.array, uintptr(n*int(width)), callerpc, pc)
	}
	if msanenabled {
		msanwrite(to.array, uintptr(n*int(width)))
		msanread(fm.array, uintptr(n*int(width)))
	}

	size := uintptr(n) * width
	if size == 1 { // common case worth about 2x to do here
		// TODO: is this still worth it with new memmove impl?
		*(*byte)(to.array) = *(*byte)(fm.array) // known to be a byte pointer
	} else {
		memmove(to.array, fm.array, size)
	}
	return n
}

func slicestringcopy(to []byte, fm string) int {
	if len(fm) == 0 || len(to) == 0 {
		return 0
	}

	n := len(fm)
	if len(to) < n {
		n = len(to)
	}

	if raceenabled {
		callerpc := getcallerpc(unsafe.Pointer(&to))
		pc := funcPC(slicestringcopy)
		racewriterangepc(unsafe.Pointer(&to[0]), uintptr(n), callerpc, pc)
	}
	if msanenabled {
		msanwrite(unsafe.Pointer(&to[0]), uintptr(n))
	}

	memmove(unsafe.Pointer(&to[0]), stringStructOf(&fm).str, uintptr(n))
	return n
}
