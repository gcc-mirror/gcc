// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// GNU/Hurd library calls.

package syscall

// Dummy function
func raw_ptrace(request int, pid int, addr uintptr, data uintptr) Errno {
	return ENOSYS
}

//sys   Fstatfs(fd int, buf *Statfs_t) (err error)
//fstatfs(fd _C_int, buf *Statfs_t) _C_int

// For exec_unix.go.
const SYS_EXECVE = 0
