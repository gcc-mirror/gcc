// syscall_linux_s390.go -- GNU/Linux s390 specific support

// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

func (r *PtraceRegs) PC() uint64 { return uint64(r.Psw.addr) }

func (r *PtraceRegs) SetPC(pc uint64) { r.Psw.addr = uint32(pc) }

const syscall_PTRACE_PEEKUSR_AREA = 0x5000
const syscall_PTRACE_POKEUSR_AREA = 0x5001

type syscall_ptrace_area struct {
	len          uint32
	kernel_addr  uint32
	process_addr uint32
}

func PtraceGetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := syscall_ptrace_area{
		12,
		0,
		uint32(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(syscall_PTRACE_PEEKUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := syscall_ptrace_area{
		12,
		0,
		uint32(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(syscall_PTRACE_POKEUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}
