// sysfile_rtems.go -- RTEMS specific file handling.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Support for basic Unix file operations.  This file simply
// translates from Go data types to Unix data types, and handles
// errno.  FIXME: This could probably be done mechanically.

package syscall

const (
	OS = "rtems"
	// No support for async I/O in RTEMS.
	O_ASYNC = 0
)

func libc_pread(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t __asm__ ("pread")
func libc_pwrite(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t __asm__ ("pwrite")
func libc_lseek64(int, Offset_t, int) Offset_t __asm__ ("lseek")
func libc_truncate64(path *byte, length Offset_t) int __asm__ ("truncate")
func libc_ftruncate64(fd int, length Offset_t) int __asm__ ("ftruncate")
func libc_nanosleep(req *Timespec, rem *Timespec) int __asm__ ("nanosleep")

func Sleep(nsec int64) (errno int) {
	errno = 0
	ts := NsecToTimespec(nsec)
	r := libc_nanosleep(&ts, nil)
	if r < 0 {
		errno = GetErrno()
	}
	return
}
