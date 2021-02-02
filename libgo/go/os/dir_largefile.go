// dir_largefile.go -- For systems which use the large file interface for
// readdir_r.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix hurd linux solaris,386 solaris,sparc

package os

import "syscall"

//extern readdir64
func libc_readdir(*syscall.DIR) *syscall.Dirent
