// syscall_linux_s390.go -- GNU/Linux s390 specific support

// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// See the s390x version for why we don't use GETREGSET/SETREGSET

package syscall

import "unsafe"

func (r *PtraceRegs) PC() uint64 { return uint64(r.Psw.addr) }

func (r *PtraceRegs) SetPC(pc uint64) { r.Psw.addr = uint32(pc) }

func PtraceGetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := _ptrace_area{
		_sizeof_ptrace_area,
		0,
		uint32(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(PTRACE_PEEKUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := _ptrace_area{
		_sizeof_ptrace_area,
		0,
		uint32(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(PTRACE_POKEUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}
