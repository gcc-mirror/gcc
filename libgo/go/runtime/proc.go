// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/atomic"
	"unsafe"
)

// Functions temporarily called by C code.
//go:linkname newextram runtime.newextram

// Functions temporarily in C that have not yet been ported.
func allocm(*p, bool, *unsafe.Pointer, *uintptr) *m
func malg(bool, bool, *unsafe.Pointer, *uintptr) *g
func allgadd(*g)

// C functions for ucontext management.
func setGContext()
func makeGContext(*g, unsafe.Pointer, uintptr)

// main_init_done is a signal used by cgocallbackg that initialization
// has been completed. It is made before _cgo_notify_runtime_init_done,
// so all cgo calls can rely on it existing. When main_init is complete,
// it is closed, meaning cgocallbackg can reliably receive from it.
var main_init_done chan bool

// If asked to move to or from a Gscanstatus this will throw. Use the castogscanstatus
// and casfrom_Gscanstatus instead.
// casgstatus will loop if the g->atomicstatus is in a Gscan status until the routine that
// put it in the Gscan state is finished.
//go:nosplit
func casgstatus(gp *g, oldval, newval uint32) {
	if (oldval&_Gscan != 0) || (newval&_Gscan != 0) || oldval == newval {
		systemstack(func() {
			print("runtime: casgstatus: oldval=", hex(oldval), " newval=", hex(newval), "\n")
			throw("casgstatus: bad incoming values")
		})
	}

	if oldval == _Grunning && gp.gcscanvalid {
		// If oldvall == _Grunning, then the actual status must be
		// _Grunning or _Grunning|_Gscan; either way,
		// we own gp.gcscanvalid, so it's safe to read.
		// gp.gcscanvalid must not be true when we are running.
		print("runtime: casgstatus ", hex(oldval), "->", hex(newval), " gp.status=", hex(gp.atomicstatus), " gp.gcscanvalid=true\n")
		throw("casgstatus")
	}

	// See http://golang.org/cl/21503 for justification of the yield delay.
	const yieldDelay = 5 * 1000
	var nextYield int64

	// loop if gp->atomicstatus is in a scan state giving
	// GC time to finish and change the state to oldval.
	for i := 0; !atomic.Cas(&gp.atomicstatus, oldval, newval); i++ {
		if oldval == _Gwaiting && gp.atomicstatus == _Grunnable {
			systemstack(func() {
				throw("casgstatus: waiting for Gwaiting but is Grunnable")
			})
		}
		// Help GC if needed.
		// if gp.preemptscan && !gp.gcworkdone && (oldval == _Grunning || oldval == _Gsyscall) {
		// 	gp.preemptscan = false
		// 	systemstack(func() {
		// 		gcphasework(gp)
		// 	})
		// }
		// But meanwhile just yield.
		if i == 0 {
			nextYield = nanotime() + yieldDelay
		}
		if nanotime() < nextYield {
			for x := 0; x < 10 && gp.atomicstatus != oldval; x++ {
				procyield(1)
			}
		} else {
			osyield()
			nextYield = nanotime() + yieldDelay/2
		}
	}
	if newval == _Grunning && gp.gcscanvalid {
		// Run queueRescan on the system stack so it has more space.
		systemstack(func() { queueRescan(gp) })
	}
}

// needm is called when a cgo callback happens on a
// thread without an m (a thread not created by Go).
// In this case, needm is expected to find an m to use
// and return with m, g initialized correctly.
// Since m and g are not set now (likely nil, but see below)
// needm is limited in what routines it can call. In particular
// it can only call nosplit functions (textflag 7) and cannot
// do any scheduling that requires an m.
//
// In order to avoid needing heavy lifting here, we adopt
// the following strategy: there is a stack of available m's
// that can be stolen. Using compare-and-swap
// to pop from the stack has ABA races, so we simulate
// a lock by doing an exchange (via casp) to steal the stack
// head and replace the top pointer with MLOCKED (1).
// This serves as a simple spin lock that we can use even
// without an m. The thread that locks the stack in this way
// unlocks the stack by storing a valid stack head pointer.
//
// In order to make sure that there is always an m structure
// available to be stolen, we maintain the invariant that there
// is always one more than needed. At the beginning of the
// program (if cgo is in use) the list is seeded with a single m.
// If needm finds that it has taken the last m off the list, its job
// is - once it has installed its own m so that it can do things like
// allocate memory - to create a spare m and put it on the list.
//
// Each of these extra m's also has a g0 and a curg that are
// pressed into service as the scheduling stack and current
// goroutine for the duration of the cgo callback.
//
// When the callback is done with the m, it calls dropm to
// put the m back on the list.
//go:nosplit
func needm(x byte) {
	if iscgo && !cgoHasExtraM {
		// Can happen if C/C++ code calls Go from a global ctor.
		// Can not throw, because scheduler is not initialized yet.
		write(2, unsafe.Pointer(&earlycgocallback[0]), int32(len(earlycgocallback)))
		exit(1)
	}

	// Lock extra list, take head, unlock popped list.
	// nilokay=false is safe here because of the invariant above,
	// that the extra list always contains or will soon contain
	// at least one m.
	mp := lockextra(false)

	// Set needextram when we've just emptied the list,
	// so that the eventual call into cgocallbackg will
	// allocate a new m for the extra list. We delay the
	// allocation until then so that it can be done
	// after exitsyscall makes sure it is okay to be
	// running at all (that is, there's no garbage collection
	// running right now).
	mp.needextram = mp.schedlink == 0
	unlockextra(mp.schedlink.ptr())

	// Save and block signals before installing g.
	// Once g is installed, any incoming signals will try to execute,
	// but we won't have the sigaltstack settings and other data
	// set up appropriately until the end of minit, which will
	// unblock the signals. This is the same dance as when
	// starting a new m to run Go code via newosproc.
	msigsave(mp)
	sigblock()

	// Install g (= m->curg).
	setg(mp.curg)
	atomic.Store(&mp.curg.atomicstatus, _Gsyscall)
	setGContext()

	// Initialize this thread to use the m.
	minit()
}

var earlycgocallback = []byte("fatal error: cgo callback before cgo call\n")

// newextram allocates m's and puts them on the extra list.
// It is called with a working local m, so that it can do things
// like call schedlock and allocate.
func newextram() {
	c := atomic.Xchg(&extraMWaiters, 0)
	if c > 0 {
		for i := uint32(0); i < c; i++ {
			oneNewExtraM()
		}
	} else {
		// Make sure there is at least one extra M.
		mp := lockextra(true)
		unlockextra(mp)
		if mp == nil {
			oneNewExtraM()
		}
	}
}

// oneNewExtraM allocates an m and puts it on the extra list.
func oneNewExtraM() {
	// Create extra goroutine locked to extra m.
	// The goroutine is the context in which the cgo callback will run.
	// The sched.pc will never be returned to, but setting it to
	// goexit makes clear to the traceback routines where
	// the goroutine stack ends.
	var g0SP unsafe.Pointer
	var g0SPSize uintptr
	mp := allocm(nil, true, &g0SP, &g0SPSize)
	gp := malg(true, false, nil, nil)
	gp.gcscanvalid = true // fresh G, so no dequeueRescan necessary
	gp.gcRescan = -1

	// malg returns status as Gidle, change to Gdead before adding to allg
	// where GC will see it.
	// gccgo uses Gdead here, not Gsyscall, because the split
	// stack context is not initialized.
	casgstatus(gp, _Gidle, _Gdead)
	gp.m = mp
	mp.curg = gp
	mp.locked = _LockInternal
	mp.lockedg = gp
	gp.lockedm = mp
	gp.goid = int64(atomic.Xadd64(&sched.goidgen, 1))
	if raceenabled {
		gp.racectx = racegostart(funcPC(newextram))
	}
	// put on allg for garbage collector
	allgadd(gp)

	// The context for gp will be set up in needm.
	// Here we need to set the context for g0.
	makeGContext(mp.g0, g0SP, g0SPSize)

	// Add m to the extra list.
	mnext := lockextra(true)
	mp.schedlink.set(mnext)
	unlockextra(mp)
}

// dropm is called when a cgo callback has called needm but is now
// done with the callback and returning back into the non-Go thread.
// It puts the current m back onto the extra list.
//
// The main expense here is the call to signalstack to release the
// m's signal stack, and then the call to needm on the next callback
// from this thread. It is tempting to try to save the m for next time,
// which would eliminate both these costs, but there might not be
// a next time: the current thread (which Go does not control) might exit.
// If we saved the m for that thread, there would be an m leak each time
// such a thread exited. Instead, we acquire and release an m on each
// call. These should typically not be scheduling operations, just a few
// atomics, so the cost should be small.
//
// TODO(rsc): An alternative would be to allocate a dummy pthread per-thread
// variable using pthread_key_create. Unlike the pthread keys we already use
// on OS X, this dummy key would never be read by Go code. It would exist
// only so that we could register at thread-exit-time destructor.
// That destructor would put the m back onto the extra list.
// This is purely a performance optimization. The current version,
// in which dropm happens on each cgo call, is still correct too.
// We may have to keep the current version on systems with cgo
// but without pthreads, like Windows.
func dropm() {
	// Clear m and g, and return m to the extra list.
	// After the call to setg we can only call nosplit functions
	// with no pointer manipulation.
	mp := getg().m

	// Block signals before unminit.
	// Unminit unregisters the signal handling stack (but needs g on some systems).
	// Setg(nil) clears g, which is the signal handler's cue not to run Go handlers.
	// It's important not to try to handle a signal between those two steps.
	sigmask := mp.sigmask
	sigblock()
	unminit()

	// gccgo sets the stack to Gdead here, because the splitstack
	// context is not initialized.
	mp.curg.atomicstatus = _Gdead
	mp.curg.gcstack = nil
	mp.curg.gcnextsp = nil

	mnext := lockextra(true)
	mp.schedlink.set(mnext)

	setg(nil)

	// Commit the release of mp.
	unlockextra(mp)

	msigrestore(sigmask)
}

// A helper function for EnsureDropM.
func getm() uintptr {
	return uintptr(unsafe.Pointer(getg().m))
}

var extram uintptr
var extraMWaiters uint32

// lockextra locks the extra list and returns the list head.
// The caller must unlock the list by storing a new list head
// to extram. If nilokay is true, then lockextra will
// return a nil list head if that's what it finds. If nilokay is false,
// lockextra will keep waiting until the list head is no longer nil.
//go:nosplit
func lockextra(nilokay bool) *m {
	const locked = 1

	incr := false
	for {
		old := atomic.Loaduintptr(&extram)
		if old == locked {
			yield := osyield
			yield()
			continue
		}
		if old == 0 && !nilokay {
			if !incr {
				// Add 1 to the number of threads
				// waiting for an M.
				// This is cleared by newextram.
				atomic.Xadd(&extraMWaiters, 1)
				incr = true
			}
			usleep(1)
			continue
		}
		if atomic.Casuintptr(&extram, old, locked) {
			return (*m)(unsafe.Pointer(old))
		}
		yield := osyield
		yield()
		continue
	}
}

//go:nosplit
func unlockextra(mp *m) {
	atomic.Storeuintptr(&extram, uintptr(unsafe.Pointer(mp)))
}
