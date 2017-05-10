// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package net

import "syscall"

func setIPv4MulticastInterface(fd *netFD, ifi *Interface) error {
	return syscall.ENOPROTOOPT
}

func setIPv4MulticastLoopback(fd *netFD, v bool) error {
	return syscall.ENOPROTOOPT
}
