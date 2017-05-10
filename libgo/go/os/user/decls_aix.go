// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package user

import "syscall"

// Declarations for the libc functions on AIX.

//extern _posix_getpwnam_r
func libc_getpwnam_r(name *byte, pwd *syscall.Passwd, buf *byte, buflen syscall.Size_t, result **syscall.Passwd) int

//extern _posix_getpwuid_r
func libc_getpwuid_r(uid syscall.Uid_t, pwd *syscall.Passwd, buf *byte, buflen syscall.Size_t, result **syscall.Passwd) int

//extern _posix_getgrnam_r
func libc_getgrnam_r(name *byte, grp *syscall.Group, buf *byte, buflen syscall.Size_t, result **syscall.Group) int

//extern _posix_getgrgid_r
func libc_getgrgid_r(gid syscall.Gid_t, grp *syscall.Group, buf *byte, buflen syscall.Size_t, result **syscall.Group) int

//extern getgrset
func libc_getgrset(user *byte) *byte
