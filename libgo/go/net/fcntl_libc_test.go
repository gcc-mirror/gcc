// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build aix || darwin || solaris

package net

import (
	"syscall"
	_ "unsafe"
)

// Use a helper function to call fcntl.  This is defined in C in
// libgo/runtime.
//extern __go_fcntl_uintptr
func libc_fcntl(uintptr, uintptr, uintptr) (uintptr, uintptr)

// Implemented in the syscall package.
//go:linkname fcntl syscall.fcntl
func fcntl(fd int, cmd int, arg int) (int, error) {
	syscall.Entersyscall()
	r, e := libc_fcntl(uintptr(fd), uintptr(cmd), uintptr(arg))
	syscall.Exitsyscall()
	if e != 0 {
		return int(r), syscall.Errno(e)
	}
	return int(r), nil
}
