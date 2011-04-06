// wait4.go -- Wait4 for systems with wait4.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func libc_wait4(Pid_t, *int, int, *Rusage) Pid_t __asm__ ("wait4")

func Wait4(pid int, wstatus *WaitStatus, options int, rusage *Rusage) (wpid int, errno int) {
	var status int
	r := libc_wait4(Pid_t(pid), &status, options, rusage)
	wpid = int(r)
	if r < 0 {
		errno = GetErrno()
	}
	if wstatus != nil {
		*wstatus = WaitStatus(status)
	}
	return
}
