// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"internal/abi"
	"internal/goarch"
	"runtime/internal/atomic"
	"unsafe"
)

type mOS struct {
	waitsemacount uint32
}

func getProcID() uint64 {
	return uint64(lwp_self())
}

//extern-sysinfo _lwp_self
func lwp_self() int32

//go:noescape
//extern-sysinfo _lwp_park
func lwp_park(ts int32, rel int32, abstime *timespec, unpark int32, hint, unparkhint unsafe.Pointer) int32

//go:noescape
//extern-sysinfo _lwp_unpark
func lwp_unpark(lwp int32, hint unsafe.Pointer) int32

//go:noescape
//extern-sysinfo sysctl
func sysctl(*uint32, uint32, *byte, *uintptr, *byte, uintptr) int32

func sysctlInt(mib []uint32) (int32, bool) {
	var out int32
	nout := unsafe.Sizeof(out)
	ret := sysctl(&mib[0], uint32(len(mib)), (*byte)(unsafe.Pointer(&out)), &nout, nil, 0)
	if ret < 0 {
		return 0, false
	}
	return out, true
}

func getncpu() int32 {
	if n, ok := sysctlInt([]uint32{_CTL_HW, _HW_NCPUONLINE}); ok {
		return int32(n)
	}
	if n, ok := sysctlInt([]uint32{_CTL_HW, _HW_NCPU}); ok {
		return int32(n)
	}
	return 1
}

func getPageSize() uintptr {
	mib := [2]uint32{_CTL_HW, _HW_PAGESIZE}
	out := uint32(0)
	nout := unsafe.Sizeof(out)
	ret := sysctl(&mib[0], 2, (*byte)(unsafe.Pointer(&out)), &nout, nil, 0)
	if ret >= 0 {
		return uintptr(out)
	}
	return 0
}

func getOSRev() int {
	if osrev, ok := sysctlInt([]uint32{_CTL_KERN, _KERN_OSREV}); ok {
		return int(osrev)
	}
	return 0
}

//go:nosplit
func semacreate(mp *m) {
}

//go:nosplit
func semasleep(ns int64) int32 {
	_g_ := getg()
	var deadline int64
	if ns >= 0 {
		deadline = nanotime() + ns
	}

	for {
		v := atomic.Load(&_g_.m.waitsemacount)
		if v > 0 {
			if atomic.Cas(&_g_.m.waitsemacount, v, v-1) {
				return 0 // semaphore acquired
			}
			continue
		}

		// Sleep until unparked by semawakeup or timeout.
		var tsp *timespec
		var ts timespec
		if ns >= 0 {
			wait := deadline - nanotime()
			if wait <= 0 {
				return -1
			}
			ts.setNsec(wait)
			tsp = &ts
		}
		ret := lwp_park(_CLOCK_MONOTONIC, _TIMER_RELTIME, tsp, 0, unsafe.Pointer(&_g_.m.waitsemacount), nil)
		if ret != 0 && errno() == _ETIMEDOUT {
			return -1
		}
	}
}

//go:nosplit
func semawakeup(mp *m) {
	atomic.Xadd(&mp.waitsemacount, 1)
	// From NetBSD's _lwp_unpark(2) manual:
	// "If the target LWP is not currently waiting, it will return
	// immediately upon the next call to _lwp_park()."
	ret := lwp_unpark(int32(mp.procid), unsafe.Pointer(&mp.waitsemacount))
	if ret != 0 && errno() != _ESRCH {
		// semawakeup can be called on signal stack.
		systemstack(func() {
			print("thrwakeup addr=", &mp.waitsemacount, " sem=", mp.waitsemacount, " errno=", errno(), "\n")
		})
	}
}

func osinit() {
	ncpu = getncpu()
	if physPageSize == 0 {
		physPageSize = getPageSize()
	}
	needSysmonWorkaround = getOSRev() < 902000000 // NetBSD 9.2
}

func sysargs(argc int32, argv **byte) {
	n := argc + 1

	// skip over argv, envp to get to auxv
	for argv_index(argv, n) != nil {
		n++
	}

	// skip NULL separator
	n++

	// now argv+n is auxv
	auxv := (*[1 << 28]uintptr)(add(unsafe.Pointer(argv), uintptr(n)*goarch.PtrSize))
	sysauxv(auxv[:])
}

const (
	_AT_NULL   = 0 // Terminates the vector
	_AT_PAGESZ = 6 // Page size in bytes
)

func sysauxv(auxv []uintptr) {
	for i := 0; auxv[i] != _AT_NULL; i += 2 {
		tag, val := auxv[i], auxv[i+1]
		switch tag {
		case _AT_PAGESZ:
			physPageSize = val
		}
	}
}
