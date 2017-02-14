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
// split.  For gccgo, however, stack splitting is not managed by the
// Go runtime. In effect, all stacks are system stacks. So this gccgo
// version just runs the function.
func systemstack(fn func()) {
	fn()
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

// in asm_*.s
func fastrand() uint32

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

//extern mincore
func mincore(addr unsafe.Pointer, n uintptr, dst *byte) int32

//go:noescape
func jmpdefer(fv *funcval, argp uintptr)
func exit1(code int32)
func asminit()
func setg(gg *g)
func breakpoint()

// reflectcall calls fn with a copy of the n argument bytes pointed at by arg.
// After fn returns, reflectcall copies n-retoffset result bytes
// back into arg+retoffset before returning. If copying result bytes back,
// the caller should pass the argument frame type as argtype, so that
// call can execute appropriate write barriers during the copy.
// Package reflect passes a frame type. In package runtime, there is only
// one call that copies results back, in cgocallbackg1, and it does NOT pass a
// frame type, meaning there are no write barriers invoked. See that call
// site for justification.
func reflectcall(argtype *_type, fn, arg unsafe.Pointer, argsize uint32, retoffset uint32)

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
// prematurely and if there are leftover stack barriers it may panic.
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

//go:noescape
func setcallerpc(argp unsafe.Pointer, pc uintptr)

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

// argp used in Defer structs when there is no argp.
const _NoArgs = ^uintptr(0)

//go:linkname time_now time.now
func time_now() (sec int64, nsec int32)

// For gccgo, expose this for C callers.
//go:linkname unixnanotime runtime.unixnanotime
func unixnanotime() int64 {
	sec, nsec := time_now()
	return sec*1e9 + int64(nsec)
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

// newobject allocates a new object.
// For gccgo unless and until we port malloc.go.
func newobject(*_type) unsafe.Pointer

// newarray allocates a new array of objects.
// For gccgo unless and until we port malloc.go.
func newarray(*_type, int) unsafe.Pointer

// For gccgo, to communicate from the C code to the Go code.
//go:linkname setIsCgo runtime.setIsCgo
func setIsCgo() {
	iscgo = true
}

// Temporary for gccgo until we port proc.go.
//go:linkname makeMainInitDone runtime.makeMainInitDone
func makeMainInitDone() {
	main_init_done = make(chan bool)
}

// Temporary for gccgo until we port proc.go.
//go:linkname closeMainInitDone runtime.closeMainInitDone
func closeMainInitDone() {
	close(main_init_done)
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

// typedmemmove copies a typed value.
// For gccgo for now.
//go:linkname typedmemmove runtime.typedmemmove
//go:nosplit
func typedmemmove(typ *_type, dst, src unsafe.Pointer) {
	memmove(dst, src, typ.size)
}

// Temporary for gccgo until we port mbarrier.go.
//go:linkname reflect_typedmemmove reflect.typedmemmove
func reflect_typedmemmove(typ *_type, dst, src unsafe.Pointer) {
	typedmemmove(typ, dst, src)
}

// Temporary for gccgo until we port mbarrier.go.
//go:nosplit
func typedmemclr(typ *_type, ptr unsafe.Pointer) {
	memclrNoHeapPointers(ptr, typ.size)
}

// Temporary for gccgo until we port mbarrier.go.
//go:nosplit
func memclrHasPointers(ptr unsafe.Pointer, n uintptr) {
	memclrNoHeapPointers(ptr, n)
}

// Temporary for gccgo until we port mbarrier.go.
//go:linkname typedslicecopy runtime.typedslicecopy
func typedslicecopy(typ *_type, dst, src slice) int {
	n := dst.len
	if n > src.len {
		n = src.len
	}
	if n == 0 {
		return 0
	}
	memmove(dst.array, src.array, uintptr(n)*typ.size)
	return n
}

// Temporary for gccgo until we port mbarrier.go.
//go:linkname reflect_typedslicecopy reflect.typedslicecopy
func reflect_typedslicecopy(elemType *_type, dst, src slice) int {
	return typedslicecopy(elemType, dst, src)
}

// Here for gccgo until we port malloc.go.
const (
	_64bit              = 1 << (^uintptr(0) >> 63) / 2
	_MHeapMap_TotalBits = (_64bit*sys.GoosWindows)*35 + (_64bit*(1-sys.GoosWindows)*(1-sys.GoosDarwin*sys.GoarchArm64))*39 + sys.GoosDarwin*sys.GoarchArm64*31 + (1-_64bit)*32
	_MaxMem             = uintptr(1<<_MHeapMap_TotalBits - 1)
	_MaxGcproc          = 32
)

// Here for gccgo until we port malloc.go.
//extern runtime_mallocgc
func c_mallocgc(size uintptr, typ uintptr, flag uint32) unsafe.Pointer
func mallocgc(size uintptr, typ *_type, needzero bool) unsafe.Pointer {
	flag := uint32(0)
	if !needzero {
		flag = 1 << 3
	}
	return c_mallocgc(size, uintptr(unsafe.Pointer(typ)), flag)
}

// Here for gccgo until we port mgc.go.
var writeBarrier struct {
	enabled bool   // compiler emits a check of this before calling write barrier
	needed  bool   // whether we need a write barrier for current GC phase
	cgo     bool   // whether we need a write barrier for a cgo check
	alignme uint64 // guarantee alignment so that compiler can use a 32 or 64-bit load
}

func queueRescan(*g) {
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

// Here for gccgo for netpoll and Solaris.
func errno() int

// Temporary for gccgo until we port proc.go.
func entersyscall(int32)
func entersyscallblock(int32)
func exitsyscall(int32)
func gopark(func(*g, unsafe.Pointer) bool, unsafe.Pointer, string, byte, int)
func goparkunlock(*mutex, string, byte, int)

// Temporary hack for gccgo until we port the garbage collector.
func typeBitsBulkBarrier(typ *_type, dst, src, size uintptr) {}

// Here for gccgo until we port msize.go.
func roundupsize(uintptr) uintptr

// Here for gccgo until we port mgc.go.
func GC()

// For gccgo to call from C code.
//go:linkname acquireWorldsema runtime.acquireWorldsema
func acquireWorldsema() {
	semacquire(&worldsema, 0)
}

// For gccgo to call from C code.
//go:linkname releaseWorldsema runtime.releaseWorldsema
func releaseWorldsema() {
	semrelease(&worldsema)
}

// For gccgo to call from C code, so that the C code and the Go code
// can share the memstats variable for now.
//go:linkname getMstats runtime.getMstats
func getMstats() *mstats {
	return &memstats
}

// Temporary for gccgo until we port proc.go.
func setcpuprofilerate_m(hz int32)

// Temporary for gccgo until we port mem_GOOS.go.
func sysAlloc(n uintptr, sysStat *uint64) unsafe.Pointer
func sysFree(v unsafe.Pointer, n uintptr, sysStat *uint64)

// Temporary for gccgo until we port proc.go, so that the C signal
// handler can call into cpuprof.
//go:linkname cpuprofAdd runtime.cpuprofAdd
func cpuprofAdd(stk []uintptr) {
	cpuprof.add(stk)
}

// For gccgo until we port proc.go.
func Breakpoint()
func LockOSThread()
func UnlockOSThread()
func lockOSThread()
func unlockOSThread()

// Temporary for gccgo until we port malloc.go
func persistentalloc(size, align uintptr, sysStat *uint64) unsafe.Pointer

// Temporary for gccgo until we port mheap.go
func setprofilebucket(p unsafe.Pointer, b *bucket)

// Temporary for gccgo until we port mgc.go.
func setgcpercent(int32) int32

//go:linkname setGCPercent runtime_debug.setGCPercent
func setGCPercent(in int32) (out int32) {
	return setgcpercent(in)
}

// Temporary for gccgo until we port atomic_pointer.go.
//go:nosplit
func atomicstorep(ptr unsafe.Pointer, new unsafe.Pointer) {
	atomic.StorepNoWB(noescape(ptr), new)
}

// Temporary for gccgo until we port mbarrier.go
func writebarrierptr(dst *uintptr, src uintptr) {
	*dst = src
}

// Temporary for gccgo until we port malloc.go
var zerobase uintptr

//go:linkname getZerobase runtime.getZerobase
func getZerobase() *uintptr {
	return &zerobase
}

// Temporary for gccgo until we port proc.go.
func sigprof()
func goexit1()

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

// Temporary for gccgo until we port mcache.go.
func allocmcache() *mcache
func freemcache(*mcache)

// Temporary for gccgo until we port mgc.go.
// This is just so that allgadd will compile.
var work struct {
	rescan struct {
		lock mutex
		list []guintptr
	}
}

// Temporary for gccgo until we port mgc.go.
var gcBlackenEnabled uint32

// Temporary for gccgo until we port mgc.go.
func gcMarkWorkAvailable(p *p) bool {
	return false
}

// Temporary for gccgo until we port mgc.go.
var gcController gcControllerState

// Temporary for gccgo until we port mgc.go.
type gcControllerState struct {
}

// Temporary for gccgo until we port mgc.go.
func (c *gcControllerState) findRunnableGCWorker(_p_ *p) *g {
	return nil
}

// Temporary for gccgo until we port mgc.go.
var gcphase uint32

// Temporary for gccgo until we port mgc.go.
const (
	_GCoff = iota
	_GCmark
	_GCmarktermination
)

// Temporary for gccgo until we port mgc.go.
type gcMarkWorkerMode int

// Temporary for gccgo until we port mgc.go.
const (
	gcMarkWorkerDedicatedMode gcMarkWorkerMode = iota
	gcMarkWorkerFractionalMode
	gcMarkWorkerIdleMode
)

// Temporary for gccgo until we port mheap.go.
type mheap struct {
}

// Temporary for gccgo until we port mheap.go.
var mheap_ mheap

// Temporary for gccgo until we port mheap.go.
func (h *mheap) scavenge(k int32, now, limit uint64) {
}

// Temporary for gccgo until we initialize ncpu in Go.
//go:linkname setncpu runtime.setncpu
func setncpu(n int32) {
	ncpu = n
}

// Temporary for gccgo until we port malloc.go.
var physPageSize uintptr

// Temporary for gccgo until we reliably initialize physPageSize in Go.
//go:linkname setpagesize runtime.setpagesize
func setpagesize(s uintptr) {
	if physPageSize == 0 {
		physPageSize = s
	}
}

// Temporary for gccgo until we port more of proc.go.
func sigprofNonGoPC(pc uintptr) {
}

// Temporary for gccgo until we port mgc.go.
// gcMarkWorkerModeStrings are the strings labels of gcMarkWorkerModes
// to use in execution traces.
var gcMarkWorkerModeStrings = [...]string{
	"GC (dedicated)",
	"GC (fractional)",
	"GC (idle)",
}
