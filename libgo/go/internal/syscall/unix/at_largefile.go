// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build hurd linux solaris,386 solaris,sparc

package unix

import (
	"syscall"
)

//extern fstatat64
func fstatat(int32, *byte, *syscall.Stat_t, int32) int32
