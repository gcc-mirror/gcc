// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

// AllThreadsSyscall performs a syscall on each OS thread of the Go
// runtime. It first invokes the syscall on one thread. Should that
// invocation fail, it returns immediately with the error status.
// Otherwise, it invokes the syscall on all of the remaining threads
// in parallel. It will terminate the program if it observes any
// invoked syscall's return value differs from that of the first
// invocation.
//
// AllThreadsSyscall is intended for emulating simultaneous
// process-wide state changes that require consistently modifying
// per-thread state of the Go runtime.
//
// AllThreadsSyscall is unaware of any threads that are launched
// explicitly by cgo linked code, so the function always returns
// ENOTSUP in binaries that use cgo.
//
//go:uintptrescapes
func AllThreadsSyscall(trap, a1, a2, a3 uintptr) (r1, r2 uintptr, err Errno) {
	return minus1, minus1, ENOTSUP
}

// AllThreadsSyscall6 is like AllThreadsSyscall, but extended to six
// arguments.
//
//go:uintptrescapes
func AllThreadsSyscall6(trap, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2 uintptr, err Errno) {
	return minus1, minus1, ENOTSUP
}

const minus1 = ^uintptr(0)
