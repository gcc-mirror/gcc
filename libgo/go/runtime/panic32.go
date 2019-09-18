// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import _ "unsafe" // for go:linkname

// For gccgo, use go:linkname to export compiler-called functions.
//
//go:linkname goPanicExtendIndex
//go:linkname goPanicExtendIndexU
//go:linkname goPanicExtendSliceAlen
//go:linkname goPanicExtendSliceAlenU
//go:linkname goPanicExtendSliceAcap
//go:linkname goPanicExtendSliceAcapU
//go:linkname goPanicExtendSliceB
//go:linkname goPanicExtendSliceBU
//go:linkname goPanicExtendSlice3Alen
//go:linkname goPanicExtendSlice3AlenU
//go:linkname goPanicExtendSlice3Acap
//go:linkname goPanicExtendSlice3AcapU
//go:linkname goPanicExtendSlice3B
//go:linkname goPanicExtendSlice3BU
//go:linkname goPanicExtendSlice3C
//go:linkname goPanicExtendSlice3CU

// Additional index/slice error paths for 32-bit platforms.
// Used when the high word of a 64-bit index is not zero.

// failures in the comparisons for s[x], 0 <= x < y (y == len(s))
func goPanicExtendIndex(x int64, y int) {
	panicCheck1(getcallerpc(), "index out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsIndex})
}
func goPanicExtendIndexU(x uint64, y int) {
	panicCheck1(getcallerpc(), "index out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsIndex})
}

// failures in the comparisons for s[:x], 0 <= x <= y (y == len(s) or cap(s))
func goPanicExtendSliceAlen(x int64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsSliceAlen})
}
func goPanicExtendSliceAlenU(x uint64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsSliceAlen})
}
func goPanicExtendSliceAcap(x int64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsSliceAcap})
}
func goPanicExtendSliceAcapU(x uint64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsSliceAcap})
}

// failures in the comparisons for s[x:y], 0 <= x <= y
func goPanicExtendSliceB(x int64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsSliceB})
}
func goPanicExtendSliceBU(x uint64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsSliceB})
}

// failures in the comparisons for s[::x], 0 <= x <= y (y == len(s) or cap(s))
func goPanicExtendSlice3Alen(x int64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsSlice3Alen})
}
func goPanicExtendSlice3AlenU(x uint64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsSlice3Alen})
}
func goPanicExtendSlice3Acap(x int64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsSlice3Acap})
}
func goPanicExtendSlice3AcapU(x uint64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsSlice3Acap})
}

// failures in the comparisons for s[:x:y], 0 <= x <= y
func goPanicExtendSlice3B(x int64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsSlice3B})
}
func goPanicExtendSlice3BU(x uint64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsSlice3B})
}

// failures in the comparisons for s[x:y:], 0 <= x <= y
func goPanicExtendSlice3C(x int64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: x, signed: true, y: y, code: boundsSlice3C})
}
func goPanicExtendSlice3CU(x uint64, y int) {
	panicCheck1(getcallerpc(), "slice bounds out of range")
	panic(boundsError{x: int64(x), signed: false, y: y, code: boundsSlice3C})
}
