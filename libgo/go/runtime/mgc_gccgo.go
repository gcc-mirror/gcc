// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// gccgo-specific support for GC.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

// For gccgo, use go:linkname to export compiler-called functions.
//
//go:linkname gcWriteBarrier

// gcRoot is a single GC root: a variable plus a ptrmask.
//go:notinheap
type gcRoot struct {
	decl    unsafe.Pointer // Pointer to variable.
	size    uintptr        // Size of variable.
	ptrdata uintptr        // Length of gcdata.
	gcdata  *uint8         // Pointer mask.
}

// gcRootList is the set of GC roots for a package.
// The next field is used to put this all into a linked list.
// count gives the real length of the array.
type gcRootList struct {
	next  *gcRootList
	count int
	roots [1 << 26]gcRoot
}

// roots is the list of GC roots for the program.
// The compiler keeps this variable itself off the list.
var gcRoots *gcRootList

// Slice containing pointers to all reachable gcRoot's sorted by
// starting address (generated at init time from 'gcRoots').
// The compiler also keeps this variable itself off the list.
// The storage backing this slice is allocated via persistentalloc(), the
// idea being that we don't want to treat the slice itself as a global
// variable, since it points to things that don't need to be scanned
// themselves.
var gcRootsIndex []*gcRoot

// rootradixsort performs an in-place radix sort of the 'arr' rootptr slice.
// Note: not a stable sort, however we expect it to be called only on slices
// with no duplicate entries, so this should not matter.
func rootradixsort(arr []*gcRoot, lo, hi int, bit uint) {
	// Partition the array into two bins based on the values at the
	// specified bit position: 0's bin (grown from the left) and and
	// 1's bin (grown from the right). We keep two boundary markers,
	// the 0's boundary "zbb" (which grows to the right) and the 1's
	// boundary "obb" (which grows to the left). At each step we
	// examine the bit for the right-of-ZBB element: if it is zero, we
	// leave it in place and move the ZBB to the right. If the bit is
	// not zero, then we swap the ZBB and OBB elements and move the
	// OBB to the left. When this is done, the two partitions are then
	// sorted using the next lower bit.

	// 0's bin boundary, initially set to before the first element
	zbb := lo - 1
	// 1's bin boundary, set to just beyond the last element
	obb := hi + 1
	// mask to pick up bit of interest
	bmask := uintptr(1) << bit

	for obb-zbb > 1 {
		zbbval := uintptr(arr[zbb+1].decl) & bmask
		if zbbval == 0 {
			// Move zbb one to the right
			zbb++
		} else {
			// Move obb one to the left and swap
			arr[obb-1], arr[zbb+1] = arr[zbb+1], arr[obb-1]
			obb--
		}
	}

	if bit != 0 {
		// NB: in most cases there is just a single partition to visit
		// so if we wanted to reduce stack space we could check for this
		// and insert a goto back up to the top.
		if zbb-lo > 0 {
			rootradixsort(arr, lo, zbb, bit-1)
		}
		if hi-obb > 0 {
			rootradixsort(arr, obb, hi, bit-1)
		}
	}
}

//go:nowritebarrier
func createGcRootsIndex() {
	// Count roots
	nroots := 0
	gcr := gcRoots
	for gcr != nil {
		nroots += gcr.count
		gcr = gcr.next
	}

	// Construct the gcRootsIndex slice. Use non-heap storage for the array
	// backing the slice.
	sp := (*notInHeapSlice)(unsafe.Pointer(&gcRootsIndex))
	sp.array = (*notInHeap)(persistentalloc1(sys.PtrSize*uintptr(nroots), sys.PtrSize, &memstats.other_sys))
	if sp.array == nil {
		throw("runtime: cannot allocate memory")
	}
	sp.len = nroots
	sp.cap = nroots

	// Populate the roots index slice
	gcr = gcRoots
	k := 0
	for gcr != nil {
		for i := 0; i < gcr.count; i++ {
			gcRootsIndex[k] = &gcr.roots[i]
			k++
		}
		gcr = gcr.next
	}

	// Sort it by starting address.
	rootradixsort(gcRootsIndex, 0, nroots-1, sys.PtrSize*8-1)
}

// registerGCRoots is called by compiler-generated code.
//go:linkname registerGCRoots

// registerGCRoots is called by init functions to register the GC
// roots for a package.  The init functions are run sequentially at
// the start of the program, so no locking is needed.
func registerGCRoots(r *gcRootList) {
	r.next = gcRoots
	gcRoots = r
}

// checkPreempt is called when the preempt field in the running G is true.
// It preempts the goroutine if it is safe to do so.
// If preemptscan is true, this scans the stack for the garbage collector
// and carries on.
func checkPreempt() {
	gp := getg()
	if !gp.preempt || gp != gp.m.curg || gp.m.locks != 0 || gp.m.mallocing != 0 || gp.m.preemptoff != "" || gp.m.incgo {
		return
	}

	// Synchronize with scang.
	gp.scanningself = true
	casgstatus(gp, _Grunning, _Gwaiting)
	if gp.preemptscan {
		for !castogscanstatus(gp, _Gwaiting, _Gscanwaiting) {
			// Likely to be racing with the GC as
			// it sees a _Gwaiting and does the
			// stack scan. If so, gcworkdone will
			// be set and gcphasework will simply
			// return.
		}
		if !gp.gcscandone {
			mp := acquirem()
			gcw := &gp.m.p.ptr().gcw
			scanstack(gp, gcw)
			releasem(mp)
			gp.gcscandone = true
		}
		gp.preemptscan = false
		gp.preempt = false
		casfrom_Gscanstatus(gp, _Gscanwaiting, _Gwaiting)
		// This clears gcscanvalid.
		casgstatus(gp, _Gwaiting, _Grunning)
		gp.scanningself = false
		return
	}

	// Act like goroutine called runtime.Gosched.
	casgstatus(gp, _Gwaiting, _Grunning)
	gp.scanningself = false
	mcall(gopreempt_m)
}

// gcWriteBarrier implements a write barrier. This is implemented in
// assembly in the gc library, but there is no special advantage to
// doing so with gccgo.
//go:nosplit
//go:nowritebarrier
func gcWriteBarrier(dst *uintptr, src uintptr) {
	buf := &getg().m.p.ptr().wbBuf
	if !buf.putFast(src, *dst) {
		wbBufFlush(dst, src)
	}
	*dst = src
}
