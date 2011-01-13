// sysfile_regfile.go -- For systems which do not use the large file interface.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func libc_pread(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t __asm__ ("pread")
func libc_pwrite(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t __asm__ ("pwrite")
func libc_lseek(int, Offset_t, int) Offset_t __asm__ ("lseek")
func libc_truncate(path *byte, length Offset_t) int __asm__ ("truncate")
func libc_ftruncate(fd int, length Offset_t) int __asm__ ("ftruncate")
