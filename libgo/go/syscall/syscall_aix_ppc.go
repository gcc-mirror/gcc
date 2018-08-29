// syscall_aix_ppc.go -- AIX 32-bit specific support

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

// AIX does not define a specific structure but instead uses separate
// ptrace calls for the different registers.
type PtraceRegs struct {
	Gpr [32]uint32
	Iar uint32
	Msr uint32
	Cr  uint32
	Lr  uint32
	Ctr uint32
	Xer uint32
}

func (r *PtraceRegs) PC() uint64 { return uint64(r.Iar) }

func (r *PtraceRegs) SetPC(pc uint64) { r.Iar = uint32(pc) }

func PtraceGetRegs(pid int, regsout *PtraceRegs) (err error) {
	ptrace(_PT_REGSET, pid, uintptr(unsafe.Pointer(&regsout.Gpr[0])), 0, 0)
	regsout.Iar = uint32(ptrace(_PT_READ_GPR, pid, 128, 0, 0))
	regsout.Msr = uint32(ptrace(_PT_READ_GPR, pid, 129, 0, 0))
	regsout.Cr = uint32(ptrace(_PT_READ_GPR, pid, 130, 0, 0))
	regsout.Lr = uint32(ptrace(_PT_READ_GPR, pid, 131, 0, 0))
	regsout.Ctr = uint32(ptrace(_PT_READ_GPR, pid, 132, 0, 0))
	regsout.Xer = uint32(ptrace(_PT_READ_GPR, pid, 133, 0, 0))
	return nil
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (err error) {
	for i := 0; i < len(regs.Gpr); i++ {
		ptrace(_PT_WRITE_GPR, pid, uintptr(i), int(regs.Gpr[i]), 0)
	}
	ptrace(_PT_WRITE_GPR, pid, 128, int(regs.Iar), 0)
	ptrace(_PT_WRITE_GPR, pid, 129, int(regs.Msr), 0)
	ptrace(_PT_WRITE_GPR, pid, 130, int(regs.Cr), 0)
	ptrace(_PT_WRITE_GPR, pid, 131, int(regs.Lr), 0)
	ptrace(_PT_WRITE_GPR, pid, 132, int(regs.Ctr), 0)
	ptrace(_PT_WRITE_GPR, pid, 133, int(regs.Xer), 0)
	return nil
}
