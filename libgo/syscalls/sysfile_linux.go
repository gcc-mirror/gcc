// sysfile_linux.go -- Linux specific file handling.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Support for basic Unix file operations.  This file simply
// translates from Go data types to Unix data types, and handles
// errno.  FIXME: This could probably be done mechanically.

package syscall

const OS = "linux"

func libc_pread(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t __asm__ ("pread64")
func libc_pwrite(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t __asm__ ("pwrite64")
func libc_lseek64(int, Offset_t, int) Offset_t __asm__ ("lseek64")
func libc_truncate64(path *byte, length Offset_t) int __asm__ ("truncate64")
func libc_ftruncate64(fd int, length Offset_t) int __asm__ ("ftruncate64")
func libc_setgroups(size Size_t, list *Gid_t) int __asm__ ("setgroups")

func Sleep(nsec int64) (errno int) {
	tv := NsecToTimeval(nsec);
	n, err := Select(0, nil, nil, nil, &tv);
	return err;
}

func Setgroups(gids []int) (errno int) {
	if len(gids) == 0 {
		return libc_setgroups(0, nil);
	}

	a := make([]Gid_t, len(gids));
	for i, v := range gids {
		a[i] = Gid_t(v);
	}
	return libc_setgroups(Size_t(len(a)), &a[0]);
}
