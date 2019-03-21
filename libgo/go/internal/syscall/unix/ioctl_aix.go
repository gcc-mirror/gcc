// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package unix

import (
	"syscall"
	"unsafe"
)

//extern __go_ioctl_ptr
func ioctl(int32, int32, unsafe.Pointer) int32

func Ioctl(fd int, cmd int, args uintptr) (err error) {
	if ioctl(int32(fd), int32(cmd), unsafe.Pointer(args)) < 0 {
		return syscall.GetErrno()
	}
	return
}
