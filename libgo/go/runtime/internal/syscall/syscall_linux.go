// Copyright 2022 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package syscall provides the syscall primitives required for the runtime.
package syscall

// TODO(https://go.dev/issue/51087): This package is incomplete and currently
// only contains very minimal support for Linux.

//extern __go_syscall6
func syscall6(num uintptr, a1, a2, a3, a4, a5, a6 uintptr) uintptr

func getErrno() uintptr
func setErrno(uintptr)

// Syscall6 calls system call number 'num' with arguments a1-6.
func Syscall6(num, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2, errno uintptr) {
	setErrno(0)
	r := syscall6(num, a1, a2, a3, a4, a5, a6)
	errno = getErrno()
	return r, 0, errno
}
