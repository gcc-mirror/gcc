// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build aix || darwin || solaris
// +build aix darwin solaris

package unix

import "syscall"

//extern __go_fcntl_uintptr
func fcntl(uintptr, uintptr, uintptr) (uintptr, uintptr)

func IsNonblock(fd int) (nonblocking bool, err error) {
	flag, e1 := fcntl(uintptr(fd), syscall.F_GETFL, 0)
	if e1 != 0 {
		return false, syscall.Errno(e1)
	}
	return flag&syscall.O_NONBLOCK != 0, nil
}
