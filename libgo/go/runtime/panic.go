// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/atomic"
	"unsafe"
)

// For gccgo, use go:linkname to rename compiler-called functions to
// themselves, so that the compiler will export them.
//
//go:linkname deferproc runtime.deferproc
//go:linkname deferreturn runtime.deferreturn
//go:linkname setdeferretaddr runtime.setdeferretaddr
//go:linkname checkdefer runtime.checkdefer
//go:linkname gopanic runtime.gopanic
//go:linkname canrecover runtime.canrecover
//go:linkname makefuncfficanrecover runtime.makefuncfficanrecover
//go:linkname makefuncreturning runtime.makefuncreturning
//go:linkname gorecover runtime.gorecover
//go:linkname deferredrecover runtime.deferredrecover
// Temporary for C code to call:
//go:linkname throw runtime.throw

// Calling panic with one of the errors below will call errorString.Error
// which will call mallocgc to concatenate strings. That will fail if
// malloc is locked, causing a confusing error message. Throw a better
// error message instead.
func panicCheckMalloc(err error) {
	gp := getg()
	if gp != nil && gp.m != nil && gp.m.mallocing != 0 {
		throw(string(err.(errorString)))
	}
}

var indexError = error(errorString("index out of range"))

func panicindex() {
	panicCheckMalloc(indexError)
	panic(indexError)
}

var sliceError = error(errorString("slice bounds out of range"))

func panicslice() {
	panicCheckMalloc(sliceError)
	panic(sliceError)
}

var divideError = error(errorString("integer divide by zero"))

func panicdivide() {
	panicCheckMalloc(divideError)
	panic(divideError)
}

var overflowError = error(errorString("integer overflow"))

func panicoverflow() {
	panicCheckMalloc(overflowError)
	panic(overflowError)
}

var floatError = error(errorString("floating point error"))

func panicfloat() {
	panicCheckMalloc(floatError)
	panic(floatError)
}

var memoryError = error(errorString("invalid memory address or nil pointer dereference"))

func panicmem() {
	panicCheckMalloc(memoryError)
	panic(memoryError)
}

func throwinit() {
	throw("recursive call during initialization - linker skew")
}

// deferproc creates a new deferred function.
// The compiler turns a defer statement into a call to this.
// frame points into the stack frame; it is used to determine which
// deferred functions are for the current stack frame, and whether we
// have already deferred functions for this frame.
// pfn is a C function pointer.
// arg is a value to pass to pfn.
func deferproc(frame *bool, pfn uintptr, arg unsafe.Pointer) {
	n := newdefer()
	n.frame = frame
	n._panic = getg()._panic
	n.pfn = pfn
	n.arg = arg
	n.retaddr = 0
	n.makefunccanrecover = false
	n.special = false
}

// Allocate a Defer, usually using per-P pool.
// Each defer must be released with freedefer.
func newdefer() *_defer {
	var d *_defer
	gp := getg()
	pp := gp.m.p.ptr()
	if len(pp.deferpool) == 0 && sched.deferpool != nil {
		systemstack(func() {
			lock(&sched.deferlock)
			for len(pp.deferpool) < cap(pp.deferpool)/2 && sched.deferpool != nil {
				d := sched.deferpool
				sched.deferpool = d.link
				d.link = nil
				pp.deferpool = append(pp.deferpool, d)
			}
			unlock(&sched.deferlock)
		})
	}
	if n := len(pp.deferpool); n > 0 {
		d = pp.deferpool[n-1]
		pp.deferpool[n-1] = nil
		pp.deferpool = pp.deferpool[:n-1]
	}
	if d == nil {
		systemstack(func() {
			d = new(_defer)
		})
	}
	d.link = gp._defer
	gp._defer = d
	return d
}

// Free the given defer.
// The defer cannot be used after this call.
//
// This must not grow the stack because there may be a frame without a
// stack map when this is called.
//
//go:nosplit
func freedefer(d *_defer) {
	if d.special {
		return
	}

	// When C code calls a Go function on a non-Go thread, the
	// deferred call to cgocallBackDone will set g to nil.
	// Don't crash trying to put d on the free list; just let it
	// be garbage collected.
	if getg() == nil {
		return
	}

	pp := getg().m.p.ptr()
	if len(pp.deferpool) == cap(pp.deferpool) {
		// Transfer half of local cache to the central cache.
		//
		// Take this slow path on the system stack so
		// we don't grow freedefer's stack.
		systemstack(func() {
			var first, last *_defer
			for len(pp.deferpool) > cap(pp.deferpool)/2 {
				n := len(pp.deferpool)
				d := pp.deferpool[n-1]
				pp.deferpool[n-1] = nil
				pp.deferpool = pp.deferpool[:n-1]
				if first == nil {
					first = d
				} else {
					last.link = d
				}
				last = d
			}
			lock(&sched.deferlock)
			last.link = sched.deferpool
			sched.deferpool = first
			unlock(&sched.deferlock)
		})
	}
	*d = _defer{}
	pp.deferpool = append(pp.deferpool, d)
}

// deferreturn is called to undefer the stack.
// The compiler inserts a call to this function as a finally clause
// wrapped around the body of any function that calls defer.
// The frame argument points to the stack frame of the function.
func deferreturn(frame *bool) {
	gp := getg()
	for gp._defer != nil && gp._defer.frame == frame {
		d := gp._defer
		pfn := d.pfn
		d.pfn = 0

		if pfn != 0 {
			// This is rather awkward.
			// The gc compiler does this using assembler
			// code in jmpdefer.
			var fn func(unsafe.Pointer)
			*(**uintptr)(unsafe.Pointer(&fn)) = &pfn
			fn(d.arg)
		}

		gp._defer = d.link

		freedefer(d)

		// Since we are executing a defer function now, we
		// know that we are returning from the calling
		// function. If the calling function, or one of its
		// callees, panicked, then the defer functions would
		// be executed by panic.
		*frame = true
	}
}

// __builtin_extract_return_addr is a GCC intrinsic that converts an
// address returned by __builtin_return_address(0) to a real address.
// On most architectures this is a nop.
//extern __builtin_extract_return_addr
func __builtin_extract_return_addr(uintptr) uintptr

// setdeferretaddr records the address to which the deferred function
// returns.  This is check by canrecover.  The frontend relies on this
// function returning false.
func setdeferretaddr(retaddr uintptr) bool {
	gp := getg()
	if gp._defer != nil {
		gp._defer.retaddr = __builtin_extract_return_addr(retaddr)
	}
	return false
}

// checkdefer is called by exception handlers used when unwinding the
// stack after a recovered panic. The exception handler is simply
//   checkdefer(frame)
//   return;
// If we have not yet reached the frame we are looking for, we
// continue unwinding.
func checkdefer(frame *bool) {
	gp := getg()
	if gp == nil {
		// We should never wind up here. Even if some other
		// language throws an exception, the cgo code
		// should ensure that g is set.
		throw("no g in checkdefer")
	} else if gp.isforeign {
		// Some other language has thrown an exception.
		// We need to run the local defer handlers.
		// If they call recover, we stop unwinding here.
		var p _panic
		p.isforeign = true
		p.link = gp._panic
		gp._panic = &p
		for {
			d := gp._defer
			if d == nil || d.frame != frame || d.pfn == 0 {
				break
			}

			pfn := d.pfn
			gp._defer = d.link

			var fn func(unsafe.Pointer)
			*(**uintptr)(unsafe.Pointer(&fn)) = &pfn
			fn(d.arg)

			freedefer(d)

			if p.recovered {
				// The recover function caught the panic
				// thrown by some other language.
				break
			}
		}

		recovered := p.recovered
		gp._panic = p.link

		if recovered {
			// Just return and continue executing Go code.
			*frame = true
			return
		}

		// We are panicking through this function.
		*frame = false
	} else if gp._defer != nil && gp._defer.pfn == 0 && gp._defer.frame == frame {
		// This is the defer function that called recover.
		// Simply return to stop the stack unwind, and let the
		// Go code continue to execute.
		d := gp._defer
		gp._defer = d.link
		freedefer(d)

		// We are returning from this function.
		*frame = true

		return
	}

	// This is some other defer function. It was already run by
	// the call to panic, or just above. Rethrow the exception.
	rethrowException()
	throw("rethrowException returned")
}

// unwindStack starts unwinding the stack for a panic. We unwind
// function calls until we reach the one which used a defer function
// which called recover. Each function which uses a defer statement
// will have an exception handler, as shown above for checkdefer.
func unwindStack() {
	// Allocate the exception type used by the unwind ABI.
	// It would be nice to define it in runtime_sysinfo.go,
	// but current definitions don't work because the required
	// alignment is larger than can be represented in Go.
	// The type never contains any Go pointers.
	size := unwindExceptionSize()
	usize := uintptr(unsafe.Sizeof(uintptr(0)))
	c := (size + usize - 1) / usize
	s := make([]uintptr, c)
	getg().exception = unsafe.Pointer(&s[0])
	throwException()
}

// Goexit terminates the goroutine that calls it. No other goroutine is affected.
// Goexit runs all deferred calls before terminating the goroutine. Because Goexit
// is not panic, however, any recover calls in those deferred functions will return nil.
//
// Calling Goexit from the main goroutine terminates that goroutine
// without func main returning. Since func main has not returned,
// the program continues execution of other goroutines.
// If all other goroutines exit, the program crashes.
func Goexit() {
	// Run all deferred functions for the current goroutine.
	// This code is similar to gopanic, see that implementation
	// for detailed comments.
	gp := getg()
	for {
		d := gp._defer
		if d == nil {
			break
		}
		gp._defer = d.link

		pfn := d.pfn
		d.pfn = 0

		if pfn != 0 {
			var fn func(unsafe.Pointer)
			*(**uintptr)(unsafe.Pointer(&fn)) = &pfn
			fn(d.arg)
		}

		freedefer(d)
		// Note: we ignore recovers here because Goexit isn't a panic
	}
	goexit1()
}

// Call all Error and String methods before freezing the world.
// Used when crashing with panicking.
// This must match types handled by printany.
func preprintpanics(p *_panic) {
	defer func() {
		if recover() != nil {
			throw("panic while printing panic value")
		}
	}()
	for p != nil {
		switch v := p.arg.(type) {
		case error:
			p.arg = v.Error()
		case stringer:
			p.arg = v.String()
		}
		p = p.link
	}
}

// Print all currently active panics. Used when crashing.
func printpanics(p *_panic) {
	if p.link != nil {
		printpanics(p.link)
		print("\t")
	}
	print("panic: ")
	printany(p.arg)
	if p.recovered {
		print(" [recovered]")
	}
	print("\n")
}

// The implementation of the predeclared function panic.
func gopanic(e interface{}) {
	gp := getg()
	if gp.m.curg != gp {
		print("panic: ")
		printany(e)
		print("\n")
		throw("panic on system stack")
	}

	if gp.m.mallocing != 0 {
		print("panic: ")
		printany(e)
		print("\n")
		throw("panic during malloc")
	}
	if gp.m.preemptoff != "" {
		print("panic: ")
		printany(e)
		print("\n")
		print("preempt off reason: ")
		print(gp.m.preemptoff)
		print("\n")
		throw("panic during preemptoff")
	}
	if gp.m.locks != 0 {
		print("panic: ")
		printany(e)
		print("\n")
		throw("panic holding locks")
	}

	// The gc compiler allocates this new _panic struct on the
	// stack. We can't do that, because when a deferred function
	// recovers the panic we unwind the stack. We unlink this
	// entry before unwinding the stack, but that doesn't help in
	// the case where we panic, a deferred function recovers and
	// then panics itself, that panic is in turn recovered, and
	// unwinds the stack past this stack frame.

	p := &_panic{
		arg:  e,
		link: gp._panic,
	}
	gp._panic = p

	for {
		d := gp._defer
		if d == nil {
			break
		}

		pfn := d.pfn
		d.pfn = 0

		if pfn != 0 {
			var fn func(unsafe.Pointer)
			*(**uintptr)(unsafe.Pointer(&fn)) = &pfn
			fn(d.arg)

			if p.recovered {
				// Some deferred function called recover.
				// Stop running this panic.
				gp._panic = p.link

				// Unwind the stack by throwing an exception.
				// The compiler has arranged to create
				// exception handlers in each function
				// that uses a defer statement.  These
				// exception handlers will check whether
				// the entry on the top of the defer stack
				// is from the current function.  If it is,
				// we have unwound the stack far enough.
				unwindStack()

				throw("unwindStack returned")
			}

			// Because we executed that defer function by a panic,
			// and it did not call recover, we know that we are
			// not returning from the calling function--we are
			// panicking through it.
			*d.frame = false
		}

		gp._defer = d.link
		freedefer(d)
	}

	// ran out of deferred calls - old-school panic now
	// Because it is unsafe to call arbitrary user code after freezing
	// the world, we call preprintpanics to invoke all necessary Error
	// and String methods to prepare the panic strings before startpanic.
	preprintpanics(gp._panic)
	startpanic()
	printpanics(gp._panic)
	dopanic(0)       // should not return
	*(*int)(nil) = 0 // not reached
}

// currentDefer returns the top of the defer stack if it can be recovered.
// Otherwise it returns nil.
func currentDefer() *_defer {
	gp := getg()
	d := gp._defer
	if d == nil {
		return nil
	}

	// The panic that would be recovered is the one on the top of
	// the panic stack. We do not want to recover it if that panic
	// was on the top of the panic stack when this function was
	// deferred.
	if d._panic == gp._panic {
		return nil
	}

	// The deferred thunk will call setdeferretaddr. If this has
	// not happened, then we have not been called via defer, and
	// we can not recover.
	if d.retaddr == 0 {
		return nil
	}

	return d
}

// canrecover is called by a thunk to see if the real function would
// be permitted to recover a panic value. Recovering a value is
// permitted if the thunk was called directly by defer. retaddr is the
// return address of the function that is calling canrecover--that is,
// the thunk.
func canrecover(retaddr uintptr) bool {
	d := currentDefer()
	if d == nil {
		return false
	}

	ret := __builtin_extract_return_addr(retaddr)
	dret := d.retaddr
	if ret <= dret && ret+16 >= dret {
		return true
	}

	// On some systems, in some cases, the return address does not
	// work reliably. See http://gcc.gnu.org/PR60406. If we are
	// permitted to call recover, the call stack will look like this:
	//     runtime.gopanic, runtime.deferreturn, etc.
	//     thunk to call deferred function (calls __go_set_defer_retaddr)
	//     function that calls __go_can_recover (passing return address)
	//     runtime.canrecover
	// Calling callers will skip the thunks. So if our caller's
	// caller starts with "runtime.", then we are permitted to
	// call recover.
	var locs [16]location
	if callers(2, locs[:2]) < 2 {
		return false
	}

	name := locs[1].function
	if hasprefix(name, "runtime.") {
		return true
	}

	// If the function calling recover was created by reflect.MakeFunc,
	// then makefuncfficanrecover will have set makefunccanrecover.
	if !d.makefunccanrecover {
		return false
	}

	// We look up the stack, ignoring libffi functions and
	// functions in the reflect package, until we find
	// reflect.makeFuncStub or reflect.ffi_callback called by FFI
	// functions.  Then we check the caller of that function.

	n := callers(3, locs[:])
	foundFFICallback := false
	i := 0
	for ; i < n; i++ {
		name = locs[i].function
		if name == "" {
			// No function name means this caller isn't Go code.
			// Assume that this is libffi.
			continue
		}

		// Ignore function in libffi.
		if hasprefix(name, "ffi_") {
			continue
		}

		if foundFFICallback {
			break
		}

		if name == "reflect.ffi_callback" {
			foundFFICallback = true
			continue
		}

		// Ignore other functions in the reflect package.
		if hasprefix(name, "reflect.") {
			continue
		}

		// We should now be looking at the real caller.
		break
	}

	if i < n {
		name = locs[i].function
		if hasprefix(name, "runtime.") {
			return true
		}
	}

	return false
}

// This function is called when code is about to enter a function
// created by the libffi version of reflect.MakeFunc. This function is
// passed the names of the callers of the libffi code that called the
// stub. It uses them to decide whether it is permitted to call
// recover, and sets d.makefunccanrecover so that gorecover can make
// the same decision.
func makefuncfficanrecover(loc []location) {
	d := currentDefer()
	if d == nil {
		return
	}

	// If we are already in a call stack of MakeFunc functions,
	// there is nothing we can usefully check here.
	if d.makefunccanrecover {
		return
	}

	// loc starts with the caller of our caller. That will be a thunk.
	// If its caller was a function function, then it was called
	// directly by defer.
	if len(loc) < 2 {
		return
	}

	name := loc[1].function
	if hasprefix(name, "runtime.") {
		d.makefunccanrecover = true
	}
}

// makefuncreturning is called when code is about to exit a function
// created by reflect.MakeFunc. It is called by the function stub used
// by reflect.MakeFunc. It clears the makefunccanrecover field. It's
// OK to always clear this field, because canrecover will only be
// called by a stub created for a function that calls recover. That
// stub will not call a function created by reflect.MakeFunc, so by
// the time we get here any caller higher up on the call stack no
// longer needs the information.
func makefuncreturning() {
	d := getg()._defer
	if d != nil {
		d.makefunccanrecover = false
	}
}

// The implementation of the predeclared function recover.
func gorecover() interface{} {
	gp := getg()
	p := gp._panic
	if p != nil && !p.recovered {
		p.recovered = true
		return p.arg
	}
	return nil
}

// deferredrecover is called when a call to recover is deferred.  That
// is, something like
//   defer recover()
//
// We need to handle this specially.  In gc, the recover function
// looks up the stack frame. In particular, that means that a deferred
// recover will not recover a panic thrown in the same function that
// defers the recover. It will only recover a panic thrown in a
// function that defers the deferred call to recover.
//
// In other words:
//
// func f1() {
// 	defer recover()	// does not stop panic
// 	panic(0)
// }
//
// func f2() {
// 	defer func() {
// 		defer recover()	// stops panic(0)
// 	}()
// 	panic(0)
// }
//
// func f3() {
// 	defer func() {
// 		defer recover()	// does not stop panic
// 		panic(0)
// 	}()
// 	panic(1)
// }
//
// func f4() {
// 	defer func() {
// 		defer func() {
// 			defer recover()	// stops panic(0)
// 		}()
// 		panic(0)
// 	}()
// 	panic(1)
// }
//
// The interesting case here is f3. As can be seen from f2, the
// deferred recover could pick up panic(1). However, this does not
// happen because it is blocked by the panic(0).
//
// When a function calls recover, then when we invoke it we pass a
// hidden parameter indicating whether it should recover something.
// This parameter is set based on whether the function is being
// invoked directly from defer. The parameter winds up determining
// whether __go_recover or __go_deferred_recover is called at all.
//
// In the case of a deferred recover, the hidden parameter that
// controls the call is actually the one set up for the function that
// runs the defer recover() statement. That is the right thing in all
// the cases above except for f3. In f3 the function is permitted to
// call recover, but the deferred recover call is not. We address that
// here by checking for that specific case before calling recover. If
// this function was deferred when there is already a panic on the
// panic stack, then we can only recover that panic, not any other.

// Note that we can get away with using a special function here
// because you are not permitted to take the address of a predeclared
// function like recover.
func deferredrecover() interface{} {
	gp := getg()
	if gp._defer == nil || gp._defer._panic != gp._panic {
		return nil
	}
	return gorecover()
}

//go:linkname sync_throw sync.throw
func sync_throw(s string) {
	throw(s)
}

//go:nosplit
func throw(s string) {
	print("fatal error: ", s, "\n")
	gp := getg()
	if gp.m.throwing == 0 {
		gp.m.throwing = 1
	}
	startpanic()
	dopanic(0)
	*(*int)(nil) = 0 // not reached
}

//uint32 runtime·panicking;
var paniclk mutex

func startpanic() {
	_g_ := getg()
	// Uncomment when mheap_ is in Go.
	// if mheap_.cachealloc.size == 0 { // very early
	//	print("runtime: panic before malloc heap initialized\n")
	//	_g_.m.mallocing = 1 // tell rest of panic not to try to malloc
	// } else
	if _g_.m.mcache == nil { // can happen if called from signal handler or throw
		_g_.m.mcache = allocmcache()
	}

	switch _g_.m.dying {
	case 0:
		_g_.m.dying = 1
		_g_.writebuf = nil
		atomic.Xadd(&panicking, 1)
		lock(&paniclk)
		if debug.schedtrace > 0 || debug.scheddetail > 0 {
			schedtrace(true)
		}
		freezetheworld()
		return
	case 1:
		// Something failed while panicking, probably the print of the
		// argument to panic().  Just print a stack trace and exit.
		_g_.m.dying = 2
		print("panic during panic\n")
		dopanic(0)
		exit(3)
		fallthrough
	case 2:
		// This is a genuine bug in the runtime, we couldn't even
		// print the stack trace successfully.
		_g_.m.dying = 3
		print("stack trace unavailable\n")
		exit(4)
		fallthrough
	default:
		// Can't even print!  Just exit.
		exit(5)
	}
}

var didothers bool
var deadlock mutex

func dopanic(unused int) {
	gp := getg()
	if gp.sig != 0 {
		signame := signame(gp.sig)
		if signame != "" {
			print("[signal ", signame)
		} else {
			print("[signal ", hex(gp.sig))
		}
		print(" code=", hex(gp.sigcode0), " addr=", hex(gp.sigcode1), " pc=", hex(gp.sigpc), "]\n")
	}

	level, all, docrash := gotraceback()
	_g_ := getg()
	if level > 0 {
		if gp != gp.m.curg {
			all = true
		}
		if gp != gp.m.g0 {
			print("\n")
			goroutineheader(gp)
			traceback(0)
		} else if level >= 2 || _g_.m.throwing > 0 {
			print("\nruntime stack:\n")
			traceback(0)
		}
		if !didothers && all {
			didothers = true
			tracebackothers(gp)
		}
	}
	unlock(&paniclk)

	if atomic.Xadd(&panicking, -1) != 0 {
		// Some other m is panicking too.
		// Let it print what it needs to print.
		// Wait forever without chewing up cpu.
		// It will exit when it's done.
		lock(&deadlock)
		lock(&deadlock)
	}

	if docrash {
		crash()
	}

	exit(2)
}

//go:nosplit
func canpanic(gp *g) bool {
	// Note that g is m->gsignal, different from gp.
	// Note also that g->m can change at preemption, so m can go stale
	// if this function ever makes a function call.
	_g_ := getg()
	_m_ := _g_.m

	// Is it okay for gp to panic instead of crashing the program?
	// Yes, as long as it is running Go code, not runtime code,
	// and not stuck in a system call.
	if gp == nil || gp != _m_.curg {
		return false
	}
	if _m_.locks-_m_.softfloat != 0 || _m_.mallocing != 0 || _m_.throwing != 0 || _m_.preemptoff != "" || _m_.dying != 0 {
		return false
	}
	status := readgstatus(gp)
	if status&^_Gscan != _Grunning || gp.syscallsp != 0 {
		return false
	}
	return true
}
