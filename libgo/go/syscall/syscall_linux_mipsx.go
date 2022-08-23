// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build linux && (mips || mipsle || mips64 || mips64le || mips64p32 || mips64p32le)

package syscall

import "unsafe"

func (r *PtraceRegs) PC() uint64 {
	return r.Cp0_epc
}

func (r *PtraceRegs) SetPC(pc uint64) {
	r.Cp0_epc = pc
}

func PtraceGetRegs(pid int, regsout *PtraceRegs) (err error) {
	return ptrace(PTRACE_GETREGS, pid, 0, uintptr(unsafe.Pointer(regsout)))
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (err error) {
	return ptrace(PTRACE_SETREGS, pid, 0, uintptr(unsafe.Pointer(regs)))
}

func rawVforkSyscall(trap, a1 uintptr) (r1 uintptr, err Errno)
