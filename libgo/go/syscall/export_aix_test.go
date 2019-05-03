// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix

package syscall

import (
	"unsafe"
)

func Ioctl(fd, req, arg uintptr) (err Errno) {
	_, err = raw_ioctl_ptr(int(fd), req, unsafe.Pointer(arg))
	return err
}
