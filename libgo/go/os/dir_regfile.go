// dir_regfile.go -- For systems which do not use the large file interface
// for readdir_r.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !aix
// +build !hurd
// +build !linux
// +build !solaris !386
// +build !solaris !sparc

package os

import "syscall"

//extern-sysinfo readdir
func libc_readdir(*syscall.DIR) *syscall.Dirent
