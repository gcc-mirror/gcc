// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !plan9
// +build !windows
// +build !nacl
// +build !js
// +build !darwin

package runtime

import "unsafe"

//go:noescape
func read(fd int32, p unsafe.Pointer, n int32) int32
func closefd(fd int32) int32

//extern exit
func exit(code int32)
func usleep(usec uint32)

//go:noescape
func write(fd uintptr, p unsafe.Pointer, n int32) int32

//go:noescape
func open(name *byte, mode, perm int32) int32

// exitThread terminates the current thread, writing *wait = 0 when
// the stack is safe to reclaim.
func exitThread(wait *uint32) {
	// This is never used by gccgo.
	throw("exitThread")
}
