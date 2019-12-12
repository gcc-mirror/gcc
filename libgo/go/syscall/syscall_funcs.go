// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin dragonfly freebsd hurd linux netbsd openbsd solaris

package syscall

//extern __go_syscall6
func syscall6(trap uintptr, a1, a2, a3, a4, a5, a6 uintptr) uintptr

// Do a system call.  We look at the size of uintptr to see how to pass
// the arguments, so that we don't pass a 64-bit value when the function
// expects a 32-bit one.
func Syscall(trap, a1, a2, a3 uintptr) (r1, r2 uintptr, err Errno) {
	Entersyscall()
	SetErrno(0)
	r := syscall6(trap, a1, a2, a3, 0, 0, 0)
	err = GetErrno()
	Exitsyscall()
	return r, 0, err
}

func Syscall6(trap, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2 uintptr, err Errno) {
	Entersyscall()
	SetErrno(0)
	r := syscall6(trap, a1, a2, a3, a4, a5, a6)
	err = GetErrno()
	Exitsyscall()
	return r, 0, err
}

func RawSyscall(trap, a1, a2, a3 uintptr) (r1, r2 uintptr, err Errno) {
	SetErrno(0)
	r := syscall6(trap, a1, a2, a3, 0, 0, 0)
	err = GetErrno()
	return r, 0, err
}

func RawSyscall6(trap, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2 uintptr, err Errno) {
	SetErrno(0)
	r := syscall6(trap, a1, a2, a3, a4, a5, a6)
	err = GetErrno()
	return r, 0, err
}
