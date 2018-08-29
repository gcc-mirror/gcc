// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package net

import (
	"runtime"
	"syscall"
	"time"
)

func setKeepAlivePeriod(fd *netFD, d time.Duration) error {
	// The kernel expects seconds so round to next highest second.
	d += (time.Second - time.Nanosecond)
	secs := int(d.Seconds())

	err := fd.pfd.SetsockoptInt(syscall.IPPROTO_TCP, syscall.SO_KEEPALIVE, secs)
	runtime.KeepAlive(fd)
	return wrapSyscallError("setsockopt", err)
}
