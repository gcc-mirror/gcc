// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package proc

import (
	"os"
	"strconv"
	"syscall"
)

type alphaRegs struct {
	syscall.PtraceRegs
	setter func(*syscall.PtraceRegs) os.Error
}

var names = [...]string{
	"r0",
	"r1",
	"r2",
	"r3",
	"r4",
	"r5",
	"r6",
	"r7",
	"r8",
	"r19",
	"r20",
	"r21",
	"r22",
	"r23",
	"r24",
	"r25",
	"r26",
	"r27",
	"r28",
	"hae",
	"trap_a0",
	"trap_a1",
	"trap_a2",
	"ps",
	"pc",
	"gp",
	"r16",
	"r17",
	"r18",
}

func (r *alphaRegs) PC() Word { return Word(r.Pc) }

func (r *alphaRegs) SetPC(val Word) os.Error {
	r.Pc = uint64(val)
	return r.setter(&r.PtraceRegs)
}

func (r *alphaRegs) Link() Word {
	panic("No link register")
}

func (r *alphaRegs) SetLink(val Word) os.Error {
	panic("No link register")
}

func (r *alphaRegs) SP() Word { return Word(r.Ps) }

func (r *alphaRegs) SetSP(val Word) os.Error {
	r.Ps = uint64(val)
	return r.setter(&r.PtraceRegs)
}

func (r *alphaRegs) Names() []string { return names[0:] }

func (r *alphaRegs) Get(i int) Word {
	switch i {
	case 0:
		return Word(r.R0)
	case 1:
		return Word(r.R1)
	case 2:
		return Word(r.R2)
	case 3:
		return Word(r.R3)
	case 4:
		return Word(r.R4)
	case 5:
		return Word(r.R5)
	case 6:
		return Word(r.R6)
	case 7:
		return Word(r.R7)
	case 8:
		return Word(r.R8)
	case 9:
		return Word(r.R19)
	case 10:
		return Word(r.R20)
	case 11:
		return Word(r.R21)
	case 12:
		return Word(r.R22)
	case 13:
		return Word(r.R23)
	case 14:
		return Word(r.R24)
	case 15:
		return Word(r.R25)
	case 16:
		return Word(r.R26)
	case 17:
		return Word(r.R27)
	case 18:
		return Word(r.R28)
	case 19:
		return Word(r.Hae)
	case 20:
		return Word(r.Trap_a0)
	case 21:
		return Word(r.Trap_a1)
	case 22:
		return Word(r.Trap_a2)
	case 23:
		return Word(r.Ps)
	case 24:
		return Word(r.Pc)
	case 25:
		return Word(r.Gp)
	case 26:
		return Word(r.R16)
	case 27:
		return Word(r.R17)
	case 28:
		return Word(r.R18)
	}
	panic("invalid register index " + strconv.Itoa(i))
}

func (r *alphaRegs) Set(i int, val Word) os.Error {
	switch i {
	case 0:
		r.R0 = uint64(val)
	case 1:
		r.R1 = uint64(val)
	case 2:
		r.R2 = uint64(val)
	case 3:
		r.R3 = uint64(val)
	case 4:
		r.R4 = uint64(val)
	case 5:
		r.R5 = uint64(val)
	case 6:
		r.R6 = uint64(val)
	case 7:
		r.R7 = uint64(val)
	case 8:
		r.R8 = uint64(val)
	case 9:
		r.R19 = uint64(val)
	case 10:
		r.R20 = uint64(val)
	case 11:
		r.R21 = uint64(val)
	case 12:
		r.R22 = uint64(val)
	case 13:
		r.R23 = uint64(val)
	case 14:
		r.R24 = uint64(val)
	case 15:
		r.R25 = uint64(val)
	case 16:
		r.R26 = uint64(val)
	case 17:
		r.R27 = uint64(val)
	case 18:
		r.R28 = uint64(val)
	case 19:
		r.Hae = uint64(val)
	case 20:
		r.Trap_a0 = uint64(val)
	case 21:
		r.Trap_a1 = uint64(val)
	case 22:
		r.Trap_a2 = uint64(val)
	case 23:
		r.Ps = uint64(val)
	case 24:
		r.Pc = uint64(val)
	case 25:
		r.Gp = uint64(val)
	case 26:
		r.R16 = uint64(val)
	case 27:
		r.R17 = uint64(val)
	case 28:
		r.R18 = uint64(val)
	default:
		panic("invalid register index " + strconv.Itoa(i))
	}
	return r.setter(&r.PtraceRegs)
}

func newRegs(regs *syscall.PtraceRegs, setter func(*syscall.PtraceRegs) os.Error) Regs {
	res := alphaRegs{}
	res.PtraceRegs = *regs
	res.setter = setter
	return &res
}
