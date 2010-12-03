// errstr.go -- Declare strerror_r for RTEMS.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

// RTEMS uses strerror_r in newlib, which is a GNU extension returning a char *.
func libc_strerror_r(int, *byte, Size_t) *byte __asm__ ("strerror_r")
