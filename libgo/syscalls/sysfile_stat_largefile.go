// sysfile_stat_largefile.go -- For systems which use the large file interface
// for *stat.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func libc_stat(name *byte, buf *Stat_t) int __asm__ ("stat64");
func libc_fstat(fd int, buf *Stat_t) int __asm__ ("fstat64");
func libc_lstat(name *byte, buf *Stat_t) int __asm__ ("lstat64");
