// syscall_solaris_sparc.go -- Solaris/SPARC specific support

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
