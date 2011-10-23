// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// For systems with the wait4 library call.

package syscall

//sys	wait4(pid Pid_t, status *int, options int, rusage *Rusage) (wpid Pid_t, errno int)
//wait4(pid Pid_t, status *int, options int, rusage *Rusage) Pid_t

func Wait4(pid int, wstatus *WaitStatus, options int, rusage *Rusage) (wpid int, errno int) {
	var status int
	r, err := wait4(Pid_t(pid), &status, options, rusage)
	wpid = int(r)
	errno = err
	if wstatus != nil {
		*wstatus = WaitStatus(status)
	}
	return
}
