// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build aix || hurd || rtems
// +build aix hurd rtems

// These are stubs.

package syscall

func Syscall(trap, a1, a2, a3 uintptr) (r1, r2 uintptr, err Errno) {
	z := -1
	return uintptr(z), 0, ENOSYS
}

func Syscall6(trap, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2 uintptr, err Errno) {
	z := -1
	return uintptr(z), 0, ENOSYS
}

func RawSyscall(trap, a1, a2, a3 uintptr) (r1, r2 uintptr, err Errno) {
	z := -1
	return uintptr(z), 0, ENOSYS
}

func RawSyscall6(trap, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2 uintptr, err Errno) {
	z := -1
	return uintptr(z), 0, ENOSYS
}
