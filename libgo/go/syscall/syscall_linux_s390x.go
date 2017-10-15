// syscall_linux_s390x.go -- GNU/Linux s390x specific support

// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

func (r *PtraceRegs) PC() uint64 { return r.Psw.addr }

func (r *PtraceRegs) SetPC(pc uint64) { r.Psw.addr = pc }

const syscall_PTRACE_PEEKUSR_AREA = 0x5000
const syscall_PTRACE_POKEUSR_AREA = 0x5001

type syscall_ptrace_area struct {
	len          uint32
	kernel_addr  uint64
	process_addr uint64
}

func PtraceGetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := syscall_ptrace_area{
		24,
		0,
		uint64(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(syscall_PTRACE_PEEKUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := syscall_ptrace_area{
		24,
		0,
		uint64(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(syscall_PTRACE_POKEUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}
