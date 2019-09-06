// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

type mOS struct{}

//go:noescape
//extern umtx_sleep
func sys_umtx_sleep(addr *uint32, val, timeout int32) int32

//go:noescape
//extern umtx_wakeup
func sys_umtx_wakeup(addr *uint32, val int32) int32

//go:noescape
//extern sysctl
func sysctl(*uint32, uint32, *byte, *uintptr, *byte, uintptr) int32

func getncpu() int32 {
	mib := [2]uint32{_CTL_HW, _HW_NCPU}
	out := uint32(0)
	nout := uintptr(unsafe.Sizeof(out))
	ret := sysctl(&mib[0], 2, (*byte)(unsafe.Pointer(&out)), &nout, nil, 0)
	if ret >= 0 {
		return int32(out)
	}
	return 1
}

func getPageSize() uintptr {
	mib := [2]uint32{_CTL_HW, _HW_PAGESIZE}
	out := uint32(0)
	nout := uintptr(unsafe.Sizeof(out))
	ret := sysctl(&mib[0], 2, (*byte)(unsafe.Pointer(&out)), &nout, nil, 0)
	if ret >= 0 {
		return uintptr(out)
	}
	return 0
}

//go:nosplit
func futexsleep(addr *uint32, val uint32, ns int64) {
	systemstack(func() {
		futexsleep1(addr, val, ns)
	})
}

func futexsleep1(addr *uint32, val uint32, ns int64) {
	var timeout int32
	if ns >= 0 {
		// The timeout is specified in microseconds - ensure that we
		// do not end up dividing to zero, which would put us to sleep
		// indefinitely...
		timeout = timediv(ns, 1000, nil)
		if timeout == 0 {
			timeout = 1
		}
	}

	// sys_umtx_sleep will return EWOULDBLOCK (EAGAIN) when the timeout
	// expires or EBUSY if the mutex value does not match.
	ret := sys_umtx_sleep(addr, int32(val), timeout)
	if ret >= 0 || ret == -_EINTR || ret == -_EAGAIN || ret == -_EBUSY {
		return
	}

	print("umtx_sleep addr=", addr, " val=", val, " ret=", ret, "\n")
	*(*int32)(unsafe.Pointer(uintptr(0x1005))) = 0x1005
}

//go:nosplit
func futexwakeup(addr *uint32, cnt uint32) {
	ret := sys_umtx_wakeup(addr, int32(cnt))
	if ret >= 0 {
		return
	}

	systemstack(func() {
		print("umtx_wake_addr=", addr, " ret=", ret, "\n")
		*(*int32)(unsafe.Pointer(uintptr(0x1006))) = 0x1006
	})
}

func osinit() {
	ncpu = getncpu()
	if physPageSize == 0 {
		physPageSize = getPageSize()
	}
}
