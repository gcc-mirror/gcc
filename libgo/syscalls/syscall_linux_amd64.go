// syscall_linux_amd64.go -- GNU/Linux 386 specific support

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func (r *PtraceRegs) PC() uint64 {
	return r.Rip;
}

func (r *PtraceRegs) SetPC(pc uint64) {
	r.Rip = pc;
}
