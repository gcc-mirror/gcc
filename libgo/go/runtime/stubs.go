// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/atomic"
	"runtime/internal/sys"
	"unsafe"
)

// Should be a built-in for unsafe.Pointer?
//go:nosplit
func add(p unsafe.Pointer, x uintptr) unsafe.Pointer {
	return unsafe.Pointer(uintptr(p) + x)
}

// getg returns the pointer to the current g.
// The compiler rewrites calls to this function into instructions
// that fetch the g directly (from TLS or from the dedicated register).
func getg() *g

// mcall switches from the g to the g0 stack and invokes fn(g),
// where g is the goroutine that made the call.
// mcall saves g's current PC/SP in g->sched so that it can be restored later.
// It is up to fn to arrange for that later execution, typically by recording
// g in a data structure, causing something to call ready(g) later.
// mcall returns to the original goroutine g later, when g has been rescheduled.
// fn must not return at all; typically it ends by calling schedule, to let the m
// run other goroutines.
//
// mcall can only be called from g stacks (not g0, not gsignal).
//
// This must NOT be go:noescape: if fn is a stack-allocated closure,
// fn puts g on a run queue, and g executes before fn returns, the
// closure will be invalidated while it is still executing.
func mcall(fn func(*g))

// systemstack runs fn on a system stack.
//
// It is common to use a func literal as the argument, in order
// to share inputs and outputs with the code around the call
// to system stack:
//
//	... set up y ...
//	systemstack(func() {
//		x = bigcall(y)
//	})
//	... use x ...
//
// For the gc toolchain this permits running a function that requires
// additional stack space in a context where the stack can not be
// split. We don't really need additional stack space in gccgo, since
// stack splitting is handled separately. But to keep things looking
// the same, we do switch to the g0 stack here if necessary.
func systemstack(fn func()) {
	gp := getg()
	mp := gp.m
	if gp == mp.g0 || gp == mp.gsignal {
		fn()
	} else if gp == mp.curg {
		mcall(func(origg *g) {
			fn()
			gogo(origg)
		})
	} else {
		badsystemstack()
	}
}

func badsystemstack() {
	throw("systemstack called from unexpected goroutine")
}

// memclrNoHeapPointers clears n bytes starting at ptr.
//
// Usually you should use typedmemclr. memclrNoHeapPointers should be
// used only when the caller knows that *ptr contains no heap pointers
// because either:
//
// 1. *ptr is initialized memory and its type is pointer-free.
//
// 2. *ptr is uninitialized memory (e.g., memory that's being reused
//    for a new allocation) and hence contains only "junk".
//
// in memclr_*.s
//go:noescape
func memclrNoHeapPointers(ptr unsafe.Pointer, n uintptr)

//go:linkname reflect_memclrNoHeapPointers reflect.memclrNoHeapPointers
func reflect_memclrNoHeapPointers(ptr unsafe.Pointer, n uintptr) {
	memclrNoHeapPointers(ptr, n)
}

// memmove copies n bytes from "from" to "to".
//go:noescape
func memmove(to, from unsafe.Pointer, n uintptr)

//go:linkname reflect_memmove reflect.memmove
func reflect_memmove(to, from unsafe.Pointer, n uintptr) {
	memmove(to, from, n)
}

//go:noescape
//extern __builtin_memcmp
func memcmp(a, b unsafe.Pointer, size uintptr) int32

// exported value for testing
var hashLoad = loadFactor

//go:nosplit
func fastrand() uint32 {
	mp := getg().m
	fr := mp.fastrand
	mx := uint32(int32(fr)>>31) & 0xa8888eef
	fr = fr<<1 ^ mx
	mp.fastrand = fr
	return fr
}

//go:nosplit
func fastrandn(n uint32) uint32 {
	// This is similar to fastrand() % n, but faster.
	// See http://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction/
	return uint32(uint64(fastrand()) * uint64(n) >> 32)
}

//go:linkname sync_fastrand sync.fastrand
func sync_fastrand() uint32 { return fastrand() }

// in asm_*.s
//go:noescape
func memequal(a, b unsafe.Pointer, size uintptr) bool

// noescape hides a pointer from escape analysis.  noescape is
// the identity function but escape analysis doesn't think the
// output depends on the input.  noescape is inlined and currently
// compiles down to zero instructions.
// USE CAREFULLY!
//go:nosplit
func noescape(p unsafe.Pointer) unsafe.Pointer {
	x := uintptr(p)
	return unsafe.Pointer(x ^ 0)
}

//go:noescape
func jmpdefer(fv *funcval, argp uintptr)
func exit1(code int32)
func setg(gg *g)

//extern __builtin_trap
func breakpoint()

func asminit() {}

//go:linkname reflectcall reflect.call
func reflectcall(fntype *functype, fn *funcval, isInterface, isMethod bool, params, results *unsafe.Pointer)

func procyield(cycles uint32)

type neverCallThisFunction struct{}

// goexit is the return stub at the top of every goroutine call stack.
// Each goroutine stack is constructed as if goexit called the
// goroutine's entry point function, so that when the entry point
// function returns, it will return to goexit, which will call goexit1
// to perform the actual exit.
//
// This function must never be called directly. Call goexit1 instead.
// gentraceback assumes that goexit terminates the stack. A direct
// call on the stack will cause gentraceback to stop walking the stack
// prematurely and if there is leftover state it may panic.
func goexit(neverCallThisFunction)

// publicationBarrier performs a store/store barrier (a "publication"
// or "export" barrier). Some form of synchronization is required
// between initializing an object and making that object accessible to
// another processor. Without synchronization, the initialization
// writes and the "publication" write may be reordered, allowing the
// other processor to follow the pointer and observe an uninitialized
// object. In general, higher-level synchronization should be used,
// such as locking or an atomic pointer write. publicationBarrier is
// for when those aren't an option, such as in the implementation of
// the memory manager.
//
// There's no corresponding barrier for the read side because the read
// side naturally has a data dependency order. All architectures that
// Go supports or seems likely to ever support automatically enforce
// data dependency ordering.
func publicationBarrier()

// getcallerpc returns the program counter (PC) of its caller's caller.
// getcallersp returns the stack pointer (SP) of its caller's caller.
// For both, the argp must be a pointer to the caller's first function argument.
// The implementation may or may not use argp, depending on
// the architecture.
//
// For example:
//
//	func f(arg1, arg2, arg3 int) {
//		pc := getcallerpc(unsafe.Pointer(&arg1))
//		sp := getcallersp(unsafe.Pointer(&arg1))
//	}
//
// These two lines find the PC and SP immediately following
// the call to f (where f will return).
//
// The call to getcallerpc and getcallersp must be done in the
// frame being asked about. It would not be correct for f to pass &arg1
// to another function g and let g call getcallerpc/getcallersp.
// The call inside g might return information about g's caller or
// information about f's caller or complete garbage.
//
// The result of getcallersp is correct at the time of the return,
// but it may be invalidated by any subsequent call to a function
// that might relocate the stack in order to grow or shrink it.
// A general rule is that the result of getcallersp should be used
// immediately and can only be passed to nosplit functions.

//go:noescape
func getcallerpc(argp unsafe.Pointer) uintptr

//go:noescape
func getcallersp(argp unsafe.Pointer) uintptr

func asmcgocall(fn, arg unsafe.Pointer) int32 {
	throw("asmcgocall")
	return 0
}

// argp used in Defer structs when there is no argp.
const _NoArgs = ^uintptr(0)

//extern __builtin_prefetch
func prefetch(addr unsafe.Pointer, rw int32, locality int32)

func prefetcht0(addr uintptr) {
	prefetch(unsafe.Pointer(addr), 0, 3)
}

func prefetcht1(addr uintptr) {
	prefetch(unsafe.Pointer(addr), 0, 2)
}

func prefetcht2(addr uintptr) {
	prefetch(unsafe.Pointer(addr), 0, 1)
}

func prefetchnta(addr uintptr) {
	prefetch(unsafe.Pointer(addr), 0, 0)
}

// round n up to a multiple of a.  a must be a power of 2.
func round(n, a uintptr) uintptr {
	return (n + a - 1) &^ (a - 1)
}

// checkASM returns whether assembly runtime checks have passed.
func checkASM() bool {
	return true
}

func eqstring(x, y string) bool {
	a := stringStructOf(&x)
	b := stringStructOf(&y)
	if a.len != b.len {
		return false
	}
	if a.str == b.str {
		return true
	}
	return memequal(a.str, b.str, uintptr(a.len))
}

// For gccgo this is in the C code.
func osyield()

// For gccgo this can be called directly.
//extern syscall
func syscall(trap uintptr, a1, a2, a3, a4, a5, a6 uintptr) uintptr

// For gccgo, to communicate from the C code to the Go code.
//go:linkname setIsCgo runtime.setIsCgo
func setIsCgo() {
	iscgo = true
}

// For gccgo, to communicate from the C code to the Go code.
//go:linkname setCpuidECX runtime.setCpuidECX
func setCpuidECX(v uint32) {
	cpuid_ecx = v
}

// For gccgo, to communicate from the C code to the Go code.
//go:linkname setSupportAES runtime.setSupportAES
func setSupportAES(v bool) {
	support_aes = v
}

// Here for gccgo until we port atomic_pointer.go and mgc.go.
//go:nosplit
func casp(ptr *unsafe.Pointer, old, new unsafe.Pointer) bool {
	if !atomic.Casp1((*unsafe.Pointer)(noescape(unsafe.Pointer(ptr))), noescape(old), new) {
		return false
	}
	return true
}

// Here for gccgo until we port lock_*.go.
func lock(l *mutex)
func unlock(l *mutex)

// Here for gccgo.
func errno() int

// Temporary for gccgo until we port proc.go.
func entersyscall(int32)
func entersyscallblock(int32)

// Here for gccgo until we port mgc.go.
func GC()

// For gccgo to call from C code, so that the C code and the Go code
// can share the memstats variable for now.
//go:linkname getMstats runtime.getMstats
func getMstats() *mstats {
	return &memstats
}

// Temporary for gccgo until we port mem_GOOS.go.
func sysAlloc(n uintptr, sysStat *uint64) unsafe.Pointer
func sysFree(v unsafe.Pointer, n uintptr, sysStat *uint64)

// Temporary for gccgo until we port malloc.go
func persistentalloc(size, align uintptr, sysStat *uint64) unsafe.Pointer

// Temporary for gccgo until we port mheap.go
func setprofilebucket(p unsafe.Pointer, b *bucket)

// Temporary for gccgo until we port atomic_pointer.go.
//go:nosplit
func atomicstorep(ptr unsafe.Pointer, new unsafe.Pointer) {
	atomic.StorepNoWB(noescape(ptr), new)
}

// Get signal trampoline, written in C.
func getSigtramp() uintptr

// The sa_handler field is generally hidden in a union, so use C accessors.
func getSigactionHandler(*_sigaction) uintptr
func setSigactionHandler(*_sigaction, uintptr)

// Retrieve fields from the siginfo_t and ucontext_t pointers passed
// to a signal handler using C, as they are often hidden in a union.
// Returns  and, if available, PC where signal occurred.
func getSiginfo(*_siginfo_t, unsafe.Pointer) (sigaddr uintptr, sigpc uintptr)

// Implemented in C for gccgo.
func dumpregs(*_siginfo_t, unsafe.Pointer)

// Temporary for gccgo until we port proc.go.
//go:linkname getsched runtime.getsched
func getsched() *schedt {
	return &sched
}

// Temporary for gccgo until we port proc.go.
//go:linkname getCgoHasExtraM runtime.getCgoHasExtraM
func getCgoHasExtraM() *bool {
	return &cgoHasExtraM
}

// Temporary for gccgo until we port proc.go.
//go:linkname getAllP runtime.getAllP
func getAllP() **p {
	return &allp[0]
}

// Temporary for gccgo until we port proc.go.
//go:linkname allocg runtime.allocg
func allocg() *g {
	return new(g)
}

// Temporary for gccgo until we port the garbage collector.
//go:linkname getallglen runtime.getallglen
func getallglen() uintptr {
	return allglen
}

// Temporary for gccgo until we port the garbage collector.
//go:linkname getallg runtime.getallg
func getallg(i int) *g {
	return allgs[i]
}

// Temporary for gccgo until we port the garbage collector.
//go:linkname getallm runtime.getallm
func getallm() *m {
	return allm
}

// Throw and rethrow an exception.
func throwException()
func rethrowException()

// Fetch the size and required alignment of the _Unwind_Exception type
// used by the stack unwinder.
func unwindExceptionSize() uintptr

// Temporary for gccgo until C code no longer needs it.
//go:nosplit
//go:linkname getPanicking runtime.getPanicking
func getPanicking() uint32 {
	return panicking
}

// Called by C code to set the number of CPUs.
//go:linkname setncpu runtime.setncpu
func setncpu(n int32) {
	ncpu = n
}

// Called by C code to set the page size.
//go:linkname setpagesize runtime.setpagesize
func setpagesize(s uintptr) {
	if physPageSize == 0 {
		physPageSize = s
	}
}

// Temporary for gccgo until we port mgc.go.
//go:linkname runtime_m0 runtime.runtime_m0
func runtime_m0() *m {
	return &m0
}

// Temporary for gccgo until we port mgc.go.
//go:linkname runtime_g0 runtime.runtime_g0
func runtime_g0() *g {
	return &g0
}

const uintptrMask = 1<<(8*sys.PtrSize) - 1

type bitvector struct {
	n        int32 // # of bits
	bytedata *uint8
}

// bool2int returns 0 if x is false or 1 if x is true.
func bool2int(x bool) int {
	if x {
		return 1
	}
	return 0
}
