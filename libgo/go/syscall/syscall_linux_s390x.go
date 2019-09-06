// syscall_linux_s390x.go -- GNU/Linux s390x specific support

// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// The PtraceRegs struct generated for go looks like this:
//
// type PtraceRegs struct
// {
//   Psw _psw_t;
//   Gprs [15+1]uint64;
//   Acrs [15+1]uint32;
//   Orig_gpr2 uint64;
//   Fp_regs _s390_fp_regs;
//   Per_info _per_struct;
//   Ieee_instruction_pointer uint64;
// }
//
// The GETREGSET/SETREGSET ptrace commands on S/390 only read/write
// the content up to Orig_gpr2.  Hence, we use
// PEEKUSR_AREA/POKEUSR_AREA like GDB does.

package syscall

import "unsafe"

func (r *PtraceRegs) PC() uint64 { return r.Psw.addr }

func (r *PtraceRegs) SetPC(pc uint64) { r.Psw.addr = pc }

func PtraceGetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := _ptrace_area{
		_sizeof_ptrace_area,
		0,
		uint64(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(PTRACE_PEEKUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (err error) {
	parea := _ptrace_area{
		_sizeof_ptrace_area,
		0,
		uint64(uintptr(unsafe.Pointer(regs))),
	}
	return ptrace(PTRACE_POKEUSR_AREA, pid, uintptr(unsafe.Pointer(&parea)), 0)
}

func rawVforkSyscall(trap, a1 uintptr) (r1 uintptr, err Errno)
