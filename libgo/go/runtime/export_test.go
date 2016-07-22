// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Export guts for testing.

package runtime

import (
	"unsafe"
)

//var Fadd64 = fadd64
//var Fsub64 = fsub64
//var Fmul64 = fmul64
//var Fdiv64 = fdiv64
//var F64to32 = f64to32
//var F32to64 = f32to64
//var Fcmp64 = fcmp64
//var Fintto64 = fintto64
//var F64toint = f64toint
//var Sqrt = sqrt

func entersyscall(int32)
func exitsyscall(int32)
func golockedOSThread() bool

var Entersyscall = entersyscall
var Exitsyscall = exitsyscall
var LockedOSThread = golockedOSThread

// var Xadduintptr = xadduintptr

// var FuncPC = funcPC

type LFNode struct {
	Next    uint64
	Pushcnt uintptr
}

func lfstackpush_go(head *uint64, node *LFNode)
func lfstackpop_go(head *uint64) *LFNode

var LFStackPush = lfstackpush_go
var LFStackPop = lfstackpop_go

type ParFor struct {
	body   func(*ParFor, uint32)
	done   uint32
	Nthr   uint32
	thrseq uint32
	Cnt    uint32
	wait   bool
}

func newParFor(nthrmax uint32) *ParFor
func parForSetup(desc *ParFor, nthr, n uint32, wait bool, body func(*ParFor, uint32))
func parForDo(desc *ParFor)
func parForIters(desc *ParFor, tid uintptr) (uintptr, uintptr)

var NewParFor = newParFor
var ParForSetup = parForSetup
var ParForDo = parForDo

func ParForIters(desc *ParFor, tid uint32) (uint32, uint32) {
	begin, end := parForIters(desc, uintptr(tid))
	return uint32(begin), uint32(end)
}

func GCMask(x interface{}) (ret []byte) {
	return nil
}

//func testSchedLocalQueue()
//func testSchedLocalQueueSteal()
//
//func RunSchedLocalQueueTest() {
//	testSchedLocalQueue()
//}
//
//func RunSchedLocalQueueStealTest() {
//	testSchedLocalQueueSteal()
//}

//var StringHash = stringHash
//var BytesHash = bytesHash
//var Int32Hash = int32Hash
//var Int64Hash = int64Hash
//var EfaceHash = efaceHash
//var IfaceHash = ifaceHash
//var MemclrBytes = memclrBytes

// var HashLoad = &hashLoad

// entry point for testing
//func GostringW(w []uint16) (s string) {
//	s = gostringw(&w[0])
//	return
//}

//var Gostringnocopy = gostringnocopy
//var Maxstring = &maxstring

//type Uintreg uintreg

//extern __go_open
func open(path *byte, mode int32, perm int32) int32

func Open(path *byte, mode int32, perm int32) int32 {
	return open(path, mode, perm)
}

//extern close
func close(int32) int32

func Close(fd int32) int32 {
	return close(fd)
}

/*
func RunSchedLocalQueueTest() {
	_p_ := new(p)
	gs := make([]g, len(_p_.runq))
	for i := 0; i < len(_p_.runq); i++ {
		if g, _ := runqget(_p_); g != nil {
			throw("runq is not empty initially")
		}
		for j := 0; j < i; j++ {
			runqput(_p_, &gs[i], false)
		}
		for j := 0; j < i; j++ {
			if g, _ := runqget(_p_); g != &gs[i] {
				print("bad element at iter ", i, "/", j, "\n")
				throw("bad element")
			}
		}
		if g, _ := runqget(_p_); g != nil {
			throw("runq is not empty afterwards")
		}
	}
}

func RunSchedLocalQueueStealTest() {
	p1 := new(p)
	p2 := new(p)
	gs := make([]g, len(p1.runq))
	for i := 0; i < len(p1.runq); i++ {
		for j := 0; j < i; j++ {
			gs[j].sig = 0
			runqput(p1, &gs[j], false)
		}
		gp := runqsteal(p2, p1, true)
		s := 0
		if gp != nil {
			s++
			gp.sig++
		}
		for {
			gp, _ = runqget(p2)
			if gp == nil {
				break
			}
			s++
			gp.sig++
		}
		for {
			gp, _ = runqget(p1)
			if gp == nil {
				break
			}
			gp.sig++
		}
		for j := 0; j < i; j++ {
			if gs[j].sig != 1 {
				print("bad element ", j, "(", gs[j].sig, ") at iter ", i, "\n")
				throw("bad element")
			}
		}
		if s != i/2 && s != i/2+1 {
			print("bad steal ", s, ", want ", i/2, " or ", i/2+1, ", iter ", i, "\n")
			throw("bad steal")
		}
	}
}

func RunSchedLocalQueueEmptyTest(iters int) {
	// Test that runq is not spuriously reported as empty.
	// Runq emptiness affects scheduling decisions and spurious emptiness
	// can lead to underutilization (both runnable Gs and idle Ps coexist
	// for arbitrary long time).
	done := make(chan bool, 1)
	p := new(p)
	gs := make([]g, 2)
	ready := new(uint32)
	for i := 0; i < iters; i++ {
		*ready = 0
		next0 := (i & 1) == 0
		next1 := (i & 2) == 0
		runqput(p, &gs[0], next0)
		go func() {
			for atomic.Xadd(ready, 1); atomic.Load(ready) != 2; {
			}
			if runqempty(p) {
				println("next:", next0, next1)
				throw("queue is empty")
			}
			done <- true
		}()
		for atomic.Xadd(ready, 1); atomic.Load(ready) != 2; {
		}
		runqput(p, &gs[1], next1)
		runqget(p)
		<-done
		runqget(p)
	}
}

var StringHash = stringHash
var BytesHash = bytesHash
var Int32Hash = int32Hash
var Int64Hash = int64Hash
var EfaceHash = efaceHash
var IfaceHash = ifaceHash
var MemclrBytes = memclrBytes
*/

//extern read
func read(fd int32, buf unsafe.Pointer, size int32) int32

func Read(fd int32, buf unsafe.Pointer, size int32) int32 {
	return read(fd, buf, size)
}

//extern write
func write(fd int32, buf unsafe.Pointer, size int32) int32

func Write(fd uintptr, buf unsafe.Pointer, size int32) int32 {
	return write(int32(fd), buf, size)
}

func envs() []string
func setenvs([]string)

var Envs = envs
var SetEnvs = setenvs

//var BigEndian = sys.BigEndian

// For benchmarking.

/*
func BenchSetType(n int, x interface{}) {
	e := *efaceOf(&x)
	t := e._type
	var size uintptr
	var p unsafe.Pointer
	switch t.kind & kindMask {
	case kindPtr:
		t = (*ptrtype)(unsafe.Pointer(t)).elem
		size = t.size
		p = e.data
	case kindSlice:
		slice := *(*struct {
			ptr      unsafe.Pointer
			len, cap uintptr
		})(e.data)
		t = (*slicetype)(unsafe.Pointer(t)).elem
		size = t.size * slice.len
		p = slice.ptr
	}
	allocSize := roundupsize(size)
	systemstack(func() {
		for i := 0; i < n; i++ {
			heapBitsSetType(uintptr(p), allocSize, size, t)
		}
	})
}

const PtrSize = sys.PtrSize

var TestingAssertE2I2GC = &testingAssertE2I2GC
var TestingAssertE2T2GC = &testingAssertE2T2GC

var ForceGCPeriod = &forcegcperiod
*/

// SetTracebackEnv is like runtime/debug.SetTraceback, but it raises
// the "environment" traceback level, so later calls to
// debug.SetTraceback (e.g., from testing timeouts) can't lower it.
func SetTracebackEnv(level string)

/*
var ReadUnaligned32 = readUnaligned32
var ReadUnaligned64 = readUnaligned64

func CountPagesInUse() (pagesInUse, counted uintptr) {
	stopTheWorld("CountPagesInUse")

	pagesInUse = uintptr(mheap_.pagesInUse)

	for _, s := range h_allspans {
		if s.state == mSpanInUse {
			counted += s.npages
		}
	}

	startTheWorld()

	return
}
*/
