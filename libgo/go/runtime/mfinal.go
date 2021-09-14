// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Garbage collector: finalizers and block profiling.

package runtime

import (
	"runtime/internal/atomic"
	"runtime/internal/sys"
	"unsafe"
)

// finblock is an array of finalizers to be executed. finblocks are
// arranged in a linked list for the finalizer queue.
//
// finblock is allocated from non-GC'd memory, so any heap pointers
// must be specially handled. GC currently assumes that the finalizer
// queue does not grow during marking (but it can shrink).
//
//go:notinheap
type finblock struct {
	alllink *finblock
	next    *finblock
	cnt     uint32
	_       int32
	fin     [(_FinBlockSize - 2*sys.PtrSize - 2*4) / unsafe.Sizeof(finalizer{})]finalizer
}

var finlock mutex  // protects the following variables
var fing *g        // goroutine that runs finalizers
var finq *finblock // list of finalizers that are to be executed
var finc *finblock // cache of free blocks
var finptrmask [_FinBlockSize / sys.PtrSize / 8]byte
var fingwait bool
var fingwake bool
var allfin *finblock // list of all blocks

// NOTE: Layout known to queuefinalizer.
type finalizer struct {
	fn  *funcval       // function to call (may be a heap pointer)
	arg unsafe.Pointer // ptr to object (may be a heap pointer)
	ft  *functype      // type of fn (unlikely, but may be a heap pointer)
	ot  *ptrtype       // type of ptr to object (may be a heap pointer)
}

func queuefinalizer(p unsafe.Pointer, fn *funcval, ft *functype, ot *ptrtype) {
	if gcphase != _GCoff {
		// Currently we assume that the finalizer queue won't
		// grow during marking so we don't have to rescan it
		// during mark termination. If we ever need to lift
		// this assumption, we can do it by adding the
		// necessary barriers to queuefinalizer (which it may
		// have automatically).
		throw("queuefinalizer during GC")
	}

	lock(&finlock)
	if finq == nil || finq.cnt == uint32(len(finq.fin)) {
		if finc == nil {
			finc = (*finblock)(persistentalloc(_FinBlockSize, 0, &memstats.gcMiscSys))
			finc.alllink = allfin
			allfin = finc
			if finptrmask[0] == 0 {
				// Build pointer mask for Finalizer array in block.
				// We allocate values of type finalizer in
				// finblock values. Since these values are
				// allocated by persistentalloc, they require
				// special scanning during GC. finptrmask is a
				// pointer mask to use while scanning.
				// Since all the values in finalizer are
				// pointers, just turn all bits on.
				for i := range finptrmask {
					finptrmask[i] = 0xff
				}
			}
		}
		block := finc
		finc = block.next
		block.next = finq
		finq = block
	}
	f := &finq.fin[finq.cnt]
	atomic.Xadd(&finq.cnt, +1) // Sync with markroots
	f.fn = fn
	f.ft = ft
	f.ot = ot
	f.arg = p
	fingwake = true
	unlock(&finlock)
}

//go:nowritebarrier
func iterate_finq(callback func(*funcval, unsafe.Pointer, *functype, *ptrtype)) {
	for fb := allfin; fb != nil; fb = fb.alllink {
		for i := uint32(0); i < fb.cnt; i++ {
			f := &fb.fin[i]
			callback(f.fn, f.arg, f.ft, f.ot)
		}
	}
}

func wakefing() *g {
	var res *g
	lock(&finlock)
	if fingwait && fingwake {
		fingwait = false
		fingwake = false
		res = fing
	}
	unlock(&finlock)
	return res
}

var (
	fingCreate uint32
)

func createfing() {
	// start the finalizer goroutine exactly once
	if fingCreate == 0 && atomic.Cas(&fingCreate, 0, 1) {
		expectSystemGoroutine()
		go runfinq()
	}
}

// This is the goroutine that runs all of the finalizers
func runfinq() {
	setSystemGoroutine()

	var (
		ef   eface
		ifac iface
	)

	gp := getg()
	gp.isFinalizerGoroutine = true
	for {
		lock(&finlock)
		fb := finq
		finq = nil
		if fb == nil {
			fing = gp
			fingwait = true
			goparkunlock(&finlock, waitReasonFinalizerWait, traceEvGoBlock, 1)
			continue
		}
		unlock(&finlock)
		for fb != nil {
			for i := fb.cnt; i > 0; i-- {
				f := &fb.fin[i-1]

				if f.ft == nil {
					throw("missing type in runfinq")
				}
				fint := f.ft.in[0]
				var param unsafe.Pointer
				switch fint.kind & kindMask {
				case kindPtr:
					// direct use of pointer
					param = unsafe.Pointer(&f.arg)
				case kindInterface:
					ityp := (*interfacetype)(unsafe.Pointer(fint))
					if len(ityp.methods) == 0 {
						// set up with empty interface
						ef._type = &f.ot.typ
						ef.data = f.arg
						param = unsafe.Pointer(&ef)
					} else {
						// convert to interface with methods
						// this conversion is guaranteed to succeed - we checked in SetFinalizer
						ifac.tab = getitab(fint, &f.ot.typ, true)
						ifac.data = f.arg
						param = unsafe.Pointer(&ifac)
					}
				default:
					throw("bad kind in runfinq")
				}
				// This is not a system goroutine while
				// running the actual finalizer.
				// This matters because we want this
				// goroutine to appear in a stack dump
				// if the finalizer crashes.
				// The gc toolchain handles this using
				// a global variable fingRunning,
				// but we don't need that.
				gp.isSystemGoroutine = false
				reflectcall(f.ft, f.fn, false, false, &param, nil)
				gp.isSystemGoroutine = true

				// Drop finalizer queue heap references
				// before hiding them from markroot.
				// This also ensures these will be
				// clear if we reuse the finalizer.
				f.fn = nil
				f.arg = nil
				f.ot = nil
				atomic.Store(&fb.cnt, i-1)
			}
			next := fb.next
			lock(&finlock)
			fb.next = finc
			finc = fb
			unlock(&finlock)
			fb = next
		}
	}
}

// SetFinalizer sets the finalizer associated with obj to the provided
// finalizer function. When the garbage collector finds an unreachable block
// with an associated finalizer, it clears the association and runs
// finalizer(obj) in a separate goroutine. This makes obj reachable again,
// but now without an associated finalizer. Assuming that SetFinalizer
// is not called again, the next time the garbage collector sees
// that obj is unreachable, it will free obj.
//
// SetFinalizer(obj, nil) clears any finalizer associated with obj.
//
// The argument obj must be a pointer to an object allocated by calling
// new, by taking the address of a composite literal, or by taking the
// address of a local variable.
// The argument finalizer must be a function that takes a single argument
// to which obj's type can be assigned, and can have arbitrary ignored return
// values. If either of these is not true, SetFinalizer may abort the
// program.
//
// Finalizers are run in dependency order: if A points at B, both have
// finalizers, and they are otherwise unreachable, only the finalizer
// for A runs; once A is freed, the finalizer for B can run.
// If a cyclic structure includes a block with a finalizer, that
// cycle is not guaranteed to be garbage collected and the finalizer
// is not guaranteed to run, because there is no ordering that
// respects the dependencies.
//
// The finalizer is scheduled to run at some arbitrary time after the
// program can no longer reach the object to which obj points.
// There is no guarantee that finalizers will run before a program exits,
// so typically they are useful only for releasing non-memory resources
// associated with an object during a long-running program.
// For example, an os.File object could use a finalizer to close the
// associated operating system file descriptor when a program discards
// an os.File without calling Close, but it would be a mistake
// to depend on a finalizer to flush an in-memory I/O buffer such as a
// bufio.Writer, because the buffer would not be flushed at program exit.
//
// It is not guaranteed that a finalizer will run if the size of *obj is
// zero bytes.
//
// It is not guaranteed that a finalizer will run for objects allocated
// in initializers for package-level variables. Such objects may be
// linker-allocated, not heap-allocated.
//
// A finalizer may run as soon as an object becomes unreachable.
// In order to use finalizers correctly, the program must ensure that
// the object is reachable until it is no longer required.
// Objects stored in global variables, or that can be found by tracing
// pointers from a global variable, are reachable. For other objects,
// pass the object to a call of the KeepAlive function to mark the
// last point in the function where the object must be reachable.
//
// For example, if p points to a struct, such as os.File, that contains
// a file descriptor d, and p has a finalizer that closes that file
// descriptor, and if the last use of p in a function is a call to
// syscall.Write(p.d, buf, size), then p may be unreachable as soon as
// the program enters syscall.Write. The finalizer may run at that moment,
// closing p.d, causing syscall.Write to fail because it is writing to
// a closed file descriptor (or, worse, to an entirely different
// file descriptor opened by a different goroutine). To avoid this problem,
// call runtime.KeepAlive(p) after the call to syscall.Write.
//
// A single goroutine runs all finalizers for a program, sequentially.
// If a finalizer must run for a long time, it should do so by starting
// a new goroutine.
func SetFinalizer(obj interface{}, finalizer interface{}) {
	if debug.sbrk != 0 {
		// debug.sbrk never frees memory, so no finalizers run
		// (and we don't have the data structures to record them).
		return
	}
	e := efaceOf(&obj)
	etyp := e._type
	if etyp == nil {
		throw("runtime.SetFinalizer: first argument is nil")
	}
	if etyp.kind&kindMask != kindPtr {
		throw("runtime.SetFinalizer: first argument is " + etyp.string() + ", not pointer")
	}
	ot := (*ptrtype)(unsafe.Pointer(etyp))
	if ot.elem == nil {
		throw("nil elem type!")
	}

	// find the containing object
	base, _, _ := findObject(uintptr(e.data), 0, 0, false)

	if base == 0 {
		// 0-length objects are okay.
		if e.data == unsafe.Pointer(&zerobase) {
			return
		}

		// Global initializers might be linker-allocated.
		//	var Foo = &Object{}
		//	func main() {
		//		runtime.SetFinalizer(Foo, nil)
		//	}
		// The relevant segments are: noptrdata, data, bss, noptrbss.
		// We cannot assume they are in any order or even contiguous,
		// due to external linking.
		//
		// For gccgo we have no reliable way to detect them,
		// so we just return.
		return
	}

	if uintptr(e.data) != base {
		// As an implementation detail we allow to set finalizers for an inner byte
		// of an object if it could come from tiny alloc (see mallocgc for details).
		if ot.elem == nil || ot.elem.ptrdata != 0 || ot.elem.size >= maxTinySize {
			throw("runtime.SetFinalizer: pointer not at beginning of allocated block")
		}
	}

	f := efaceOf(&finalizer)
	ftyp := f._type
	if ftyp == nil {
		// switch to system stack and remove finalizer
		systemstack(func() {
			removefinalizer(e.data)
		})
		return
	}

	if ftyp.kind&kindMask != kindFunc {
		throw("runtime.SetFinalizer: second argument is " + ftyp.string() + ", not a function")
	}
	ft := (*functype)(unsafe.Pointer(ftyp))
	if ft.dotdotdot {
		throw("runtime.SetFinalizer: cannot pass " + etyp.string() + " to finalizer " + ftyp.string() + " because dotdotdot")
	}
	if len(ft.in) != 1 {
		throw("runtime.SetFinalizer: cannot pass " + etyp.string() + " to finalizer " + ftyp.string())
	}
	fint := ft.in[0]
	switch {
	case fint == etyp:
		// ok - same type
		goto okarg
	case fint.kind&kindMask == kindPtr:
		if (fint.uncommontype == nil || etyp.uncommontype == nil) && (*ptrtype)(unsafe.Pointer(fint)).elem == ot.elem {
			// ok - not same type, but both pointers,
			// one or the other is unnamed, and same element type, so assignable.
			goto okarg
		}
	case fint.kind&kindMask == kindInterface:
		ityp := (*interfacetype)(unsafe.Pointer(fint))
		if len(ityp.methods) == 0 {
			// ok - satisfies empty interface
			goto okarg
		}
		if getitab(fint, etyp, true) == nil {
			goto okarg
		}
	}
	throw("runtime.SetFinalizer: cannot pass " + etyp.string() + " to finalizer " + ftyp.string())
okarg:
	// make sure we have a finalizer goroutine
	createfing()

	systemstack(func() {
		data := f.data
		if !isDirectIface(ftyp) {
			data = *(*unsafe.Pointer)(data)
		}
		if !addfinalizer(e.data, (*funcval)(data), ft, ot) {
			throw("runtime.SetFinalizer: finalizer already set")
		}
	})
}

// Mark KeepAlive as noinline so that it is easily detectable as an intrinsic.
//go:noinline

// KeepAlive marks its argument as currently reachable.
// This ensures that the object is not freed, and its finalizer is not run,
// before the point in the program where KeepAlive is called.
//
// A very simplified example showing where KeepAlive is required:
// 	type File struct { d int }
// 	d, err := syscall.Open("/file/path", syscall.O_RDONLY, 0)
// 	// ... do something if err != nil ...
// 	p := &File{d}
// 	runtime.SetFinalizer(p, func(p *File) { syscall.Close(p.d) })
// 	var buf [10]byte
// 	n, err := syscall.Read(p.d, buf[:])
// 	// Ensure p is not finalized until Read returns.
// 	runtime.KeepAlive(p)
// 	// No more uses of p after this point.
//
// Without the KeepAlive call, the finalizer could run at the start of
// syscall.Read, closing the file descriptor before syscall.Read makes
// the actual system call.
//
// Note: KeepAlive should only be used to prevent finalizers from
// running prematurely. In particular, when used with unsafe.Pointer,
// the rules for valid uses of unsafe.Pointer still apply.
func KeepAlive(x interface{}) {
	// Introduce a use of x that the compiler can't eliminate.
	// This makes sure x is alive on entry. We need x to be alive
	// on entry for "defer runtime.KeepAlive(x)"; see issue 21402.
	if cgoAlwaysFalse {
		println(x)
	}
}
