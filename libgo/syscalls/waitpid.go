// waitpid.go -- Wait4 for systems without wait4, but with waitpid.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func libc_waitpid(Pid_t, *int, int) Pid_t __asm__ ("waitpid")

func Wait4(pid int, wstatus *WaitStatus, options int, rusage *Rusage) (wpid int, errno int) {
	var status int
	r := libc_waitpid(Pid_t(pid), &status, options)
	wpid = int(r)
	if r < 0 {
		errno = GetErrno()
	}
	if wstatus != nil {
		*wstatus = WaitStatus(status)
	}
	return
}
