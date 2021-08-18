// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build !js && !plan9 && !windows
// +build !js,!plan9,!windows

package runtime

import "unsafe"

// read calls the read system call.
// It returns a non-negative number of bytes written or a negative errno value.
//go:noescape
func read(fd int32, p unsafe.Pointer, n int32) int32

func closefd(fd int32) int32

//extern-sysinfo exit
func exit(code int32)
func usleep(usec uint32)

//go:nosplit
func usleep_no_g(usec uint32) {
	usleep(usec)
}

// write calls the write system call.
// It returns a non-negative number of bytes written or a negative errno value.
//go:noescape
func write1(fd uintptr, p unsafe.Pointer, n int32) int32

//go:noescape
func open(name *byte, mode, perm int32) int32

// exitThread terminates the current thread, writing *wait = 0 when
// the stack is safe to reclaim.
func exitThread(wait *uint32) {
	// This is never used by gccgo.
	throw("exitThread")
}

// So that the C initialization code can call osinit.
//go:linkname osinit runtime.osinit
