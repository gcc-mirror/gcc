// errstr_decl_linux.go -- Declare strerror_r for GNU/Linux.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func libc_strerror_r(int, *byte, Size_t) int __asm__ ("__xpg_strerror_r")
