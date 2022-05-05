// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build solaris
// +build solaris

package lif

import (
	"syscall"
	"unsafe"
)

//extern __go_ioctl_ptr
func libc_ioctl(int32, int32, unsafe.Pointer) int32

func ioctl(s, ioc uintptr, arg unsafe.Pointer) error {
	if libc_ioctl(int32(s), int32(ioc), arg) < 0 {
		return syscall.GetErrno()
	}
	return nil
}
