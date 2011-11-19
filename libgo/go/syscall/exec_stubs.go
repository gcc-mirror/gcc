// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Stubs for fork, exec and wait.

package syscall

func ForkExec(argv0 string, argv []string, envv []string, dir string, fd []int) (pid int, err int) {
	return -1, ENOSYS;
}

func Exec(argv0 string, argv []string, envv []string) (err int) {
	return ENOSYS;
}

func Wait4(pid int, wstatus *WaitStatus, options int, rusage *Rusage) (wpid int, errno int) {
	return -1, ENOSYS;
}

func raw_ptrace(request int, pid int, addr *byte, data *byte) int {
	return ENOSYS
}
