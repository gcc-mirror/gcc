// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix dragonfly freebsd hurd js,wasm linux nacl netbsd openbsd solaris

package poll

import "syscall"

// Use a helper function to call fcntl.  This is defined in C in
// libgo/runtime.
//extern __go_fcntl_uintptr
func libc_fcntl(uintptr, uintptr, uintptr) (uintptr, uintptr)

// Fsync wraps syscall.Fsync.
func (fd *FD) Fsync() error {
	if err := fd.incref(); err != nil {
		return err
	}
	defer fd.decref()
	return syscall.Fsync(fd.Sysfd)
}

func fcntl(fd int, cmd int, arg int) (int, error) {
	syscall.Entersyscall()
	r, e := libc_fcntl(uintptr(fd), uintptr(cmd), uintptr(arg))
	syscall.Exitsyscall()
	if e != 0 {
		return int(r), syscall.Errno(e)
	}
	return int(r), nil
}
