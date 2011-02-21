// dir_regfile.go -- For systems which do not use the large file interface
// for readdir_r.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package os

import "syscall"

func libc_readdir_r(*syscall.DIR, *syscall.Dirent, **syscall.Dirent) int __asm__ ("readdir_r")
