// syscall_aix_ppc64.go -- AIX 64-bit specific support

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

// AIX does not define a specific structure but instead uses separate
// ptrace calls for the different registers.
type PtraceRegs struct {
	Gpr [32]uint64
	Iar uint64
	Msr uint64
	Cr  uint64
	Lr  uint64
	Ctr uint64
	Xer uint64
}

func (r *PtraceRegs) PC() uint64 { return r.Iar }

func (r *PtraceRegs) SetPC(pc uint64) { r.Iar = pc }

func PtraceGetRegs(pid int, regsout *PtraceRegs) (err error) {
	ptrace64(_PT_REGSET, int64(pid), int64(uintptr(unsafe.Pointer(&regsout.Gpr[0]))), 0, 0)
	ptrace64(_PT_READ_GPR, int64(pid), 128, 0, uintptr(unsafe.Pointer(&regsout.Iar)))
	ptrace64(_PT_READ_GPR, int64(pid), 129, 0, uintptr(unsafe.Pointer(&regsout.Msr)))
	ptrace64(_PT_READ_GPR, int64(pid), 130, 0, uintptr(unsafe.Pointer(&regsout.Cr)))
	ptrace64(_PT_READ_GPR, int64(pid), 131, 0, uintptr(unsafe.Pointer(&regsout.Lr)))
	ptrace64(_PT_READ_GPR, int64(pid), 132, 0, uintptr(unsafe.Pointer(&regsout.Ctr)))
	ptrace64(_PT_READ_GPR, int64(pid), 133, 0, uintptr(unsafe.Pointer(&regsout.Xer)))
	return nil
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (err error) {
	for i := 0; i < len(regs.Gpr); i++ {
		ptrace64(_PT_WRITE_GPR, int64(pid), int64(i), 0, uintptr(unsafe.Pointer(&regs.Gpr[i])))
	}
	ptrace64(_PT_WRITE_GPR, int64(pid), 128, 0, uintptr(unsafe.Pointer(&regs.Iar)))
	ptrace64(_PT_WRITE_GPR, int64(pid), 129, 0, uintptr(unsafe.Pointer(&regs.Msr)))
	ptrace64(_PT_WRITE_GPR, int64(pid), 130, 0, uintptr(unsafe.Pointer(&regs.Cr)))
	ptrace64(_PT_WRITE_GPR, int64(pid), 131, 0, uintptr(unsafe.Pointer(&regs.Lr)))
	ptrace64(_PT_WRITE_GPR, int64(pid), 132, 0, uintptr(unsafe.Pointer(&regs.Ctr)))
	ptrace64(_PT_WRITE_GPR, int64(pid), 133, 0, uintptr(unsafe.Pointer(&regs.Xer)))
	return nil
}
