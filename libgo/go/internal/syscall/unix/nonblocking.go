// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin dragonfly freebsd linux netbsd openbsd solaris

package unix

import (
	"syscall"
	_ "unsafe" // for go:linkname
)

//extern __go_fcntl
func syscall_fcntl(fd int32, cmd int32, arg int32) int32

func IsNonblock(fd int) (nonblocking bool, err error) {
	flag := syscall_fcntl(int32(fd), syscall.F_GETFL, 0)
	if flag < 0 {
		return false, syscall.GetErrno()
	}
	return flag&syscall.O_NONBLOCK != 0, nil
}
