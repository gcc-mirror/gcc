// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build irix
// +build irix

package syscall

//sysnb raw_ptrace(request int, pid int, addr uintptr, data uintptr) (err Errno)
//ptrace(request _C_int, pid Pid_t, addr *byte, data *byte) _C_long
