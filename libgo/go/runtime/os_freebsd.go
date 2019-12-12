// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"unsafe"
)

type mOS struct{}

//go:noescape
//extern _umtx_op
func sys_umtx_op(addr *uint32, mode int32, val uint32, uaddr1 uinptr, ts *umtx_time) int32

//go:noescape
//extern sysctl
func sysctl(*uint32, uint32, *byte, *uintptr, *byte, uintptr) int32

const (
	_CTL_MAXNAME     = 24
	_CPU_LEVEL_WHICH = 3
	_CPU_WHICH_PID   = 2
)

// From FreeBSD's <sys/sysctl.h>
const (
	_CTL_HW      = 6
	_HW_PAGESIZE = 7
)

// Undocumented numbers from FreeBSD's lib/libc/gen/sysctlnametomib.c.
const (
	_CTL_QUERY     = 0
	_CTL_QUERY_MIB = 3
)

// sysctlnametomib fill mib with dynamically assigned sysctl entries of name,
// return count of effected mib slots, return 0 on error.
func sysctlnametomib(name []byte, mib *[_CTL_MAXNAME]uint32) uint32 {
	oid := [2]uint32{_CTL_QUERY, _CTL_QUERY_MIB}
	miblen := uintptr(_CTL_MAXNAME)
	if sysctl(&oid[0], 2, (*byte)(unsafe.Pointer(mib)), &miblen, (*byte)(unsafe.Pointer(&name[0])), (uintptr)(len(name))) < 0 {
		return 0
	}
	miblen /= unsafe.Sizeof(uint32(0))
	if miblen <= 0 {
		return 0
	}
	return uint32(miblen)
}

const (
	_CPU_CURRENT_PID = -1 // Current process ID.
)

//go:noescape
//extern cpuset_getaffinity
func cpuset_getaffinity(level int32, which int32, id int64, size uintptr, mask *byte) int32

//go:systemstack
func getncpu() int32 {
	// Use a large buffer for the CPU mask. We're on the system
	// stack, so this is fine, and we can't allocate memory for a
	// dynamically-sized buffer at this point.
	const maxCPUs = 64 * 1024
	var mask [maxCPUs / 8]byte
	var mib [_CTL_MAXNAME]uint32

	// According to FreeBSD's /usr/src/sys/kern/kern_cpuset.c,
	// cpuset_getaffinity return ERANGE when provided buffer size exceed the limits in kernel.
	// Querying kern.smp.maxcpus to calculate maximum buffer size.
	// See https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=200802

	// Variable kern.smp.maxcpus introduced at Dec 23 2003, revision 123766,
	// with dynamically assigned sysctl entries.
	miblen := sysctlnametomib([]byte("kern.smp.maxcpus"), &mib)
	if miblen == 0 {
		return 1
	}

	// Query kern.smp.maxcpus.
	dstsize := uintptr(4)
	maxcpus := uint32(0)
	if sysctl(&mib[0], miblen, (*byte)(unsafe.Pointer(&maxcpus)), &dstsize, nil, 0) != 0 {
		return 1
	}

	maskSize := uintptr(int(maxcpus+7) / 8)
	if maskSize < sys.PtrSize {
		maskSize = sys.PtrSize
	}
	if maskSize > uintptr(len(mask)) {
		maskSize = uintptr(len(mask))
	}

	if cpuset_getaffinity(_CPU_LEVEL_WHICH, _CPU_WHICH_PID, _CPU_CURRENT_PID,
		maskSize, (*byte)(unsafe.Pointer(&mask[0]))) != 0 {
		return 1
	}
	n := int32(0)
	for _, v := range mask[:maskSize] {
		for v != 0 {
			n += int32(v & 1)
			v >>= 1
		}
	}
	if n == 0 {
		return 1
	}
	return n
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

// FreeBSD's umtx_op syscall is effectively the same as Linux's futex, and
// thus the code is largely similar. See Linux implementation
// and lock_futex.go for comments.

//go:nosplit
func futexsleep(addr *uint32, val uint32, ns int64) {
	systemstack(func() {
		futexsleep1(addr, val, ns)
	})
}

func futexsleep1(addr *uint32, val uint32, ns int64) {
	var utp *umtx_time
	if ns >= 0 {
		var ut umtx_time
		ut._clockid = _CLOCK_MONOTONIC
		ut._timeout.setNsec(ns)
		utp = &ut
	}
	ret := sys_umtx_op(addr, _UMTX_OP_WAIT_UINT_PRIVATE, val, unsafe.Sizeof(*utp), utp)
	if ret >= 0 || ret == -_EINTR {
		return
	}
	print("umtx_wait addr=", addr, " val=", val, " ret=", ret, "\n")
	*(*int32)(unsafe.Pointer(uintptr(0x1005))) = 0x1005
}

//go:nosplit
func futexwakeup(addr *uint32, cnt uint32) {
	ret := sys_umtx_op(addr, _UMTX_OP_WAKE_PRIVATE, cnt, 0, nil)
	if ret >= 0 {
		return
	}

	systemstack(func() {
		print("umtx_wake_addr=", addr, " ret=", ret, "\n")
	})
}

func osinit() {
	ncpu = getncpu()
	if physPageSize == 0 {
		physPageSize = getPageSize()
	}
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
	auxv := (*[1 << 28]uintptr)(add(unsafe.Pointer(argv), uintptr(n)*sys.PtrSize))
	sysauxv(auxv[:])
}

const (
	_AT_NULL     = 0  // Terminates the vector
	_AT_PAGESZ   = 6  // Page size in bytes
	_AT_TIMEKEEP = 22 // Pointer to timehands.
	_AT_HWCAP    = 25 // CPU feature flags
	_AT_HWCAP2   = 26 // CPU feature flags 2
)

func sysauxv(auxv []uintptr) {
	for i := 0; auxv[i] != _AT_NULL; i += 2 {
		tag, val := auxv[i], auxv[i+1]
		switch tag {
		// _AT_NCPUS from auxv shouldn't be used due to golang.org/issue/15206
		case _AT_PAGESZ:
			physPageSize = val
		case _AT_TIMEKEEP:
			timekeepSharedPage = (*vdsoTimekeep)(unsafe.Pointer(val))
		}

		archauxv(tag, val)
	}
}
