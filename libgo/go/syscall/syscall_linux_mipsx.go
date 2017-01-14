// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build linux
// +build mips mipsle

package syscall

func (r *PtraceRegs) PC() uint64 { return uint64(r.Regs[64]) }

func (r *PtraceRegs) SetPC(pc uint64) { r.Regs[64] = uint32(pc) }
