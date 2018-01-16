// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// gccgo-specific support for GC.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

// gcRoot is a single GC root: a variable plus a ptrmask.
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

// registerGCRoots is called by compiler-generated code.
//go:linkname registerGCRoots runtime.registerGCRoots

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
	if !gp.preempt || gp != gp.m.curg || gp.m.locks != 0 || gp.m.mallocing != 0 || gp.m.preemptoff != "" {
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
			if gcBlackenPromptly {
				gcw.dispose()
			}
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
	next := buf.next
	np := next + 2*sys.PtrSize
	buf.next = np
	*(*uintptr)(unsafe.Pointer(next)) = src
	*(*uintptr)(unsafe.Pointer(next + sys.PtrSize)) = *dst
	if np >= buf.end {
		wbBufFlush(dst, src)
	}
	*dst = src
}
