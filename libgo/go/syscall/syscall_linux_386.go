// syscall_linux_386.go -- GNU/Linux 386 specific support

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func (r *PtraceRegs) PC() uint64 {
	return uint64(uint32(r.Eip));
}

func (r *PtraceRegs) SetPC(pc uint64) {
	r.Eip = int32(pc);
}
