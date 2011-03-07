// syscall_solaris_386.go -- Solaris/x86 specific support

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

// FIXME: ptrace(3C) has this, but exec.go expects the next.
//func libc_ptrace(request int, pid Pid_t, addr int, data int) int __asm__ ("ptrace")

func libc_ptrace(request int, pid Pid_t, addr uintptr, data *byte) int __asm__ ("ptrace")

var dummy *byte
const sizeofPtr uintptr = uintptr(unsafe.Sizeof(dummy))

// 32-bit Solaris 2/x86 needs to use _nuname internally, cf. <sys/utsname.h>.
func libc_uname(buf *Utsname) (errno int) __asm__("_nuname")
