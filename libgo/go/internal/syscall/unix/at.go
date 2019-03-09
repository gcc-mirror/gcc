// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// -build linux openbsd netbsd dragonfly

package unix

import (
	"syscall"
)

//extern unlinkat
func unlinkat(int32, *byte, int32) int32

//extern __go_openat
func openat(int32, *byte, int32, syscall.Mode_t) int32

func Unlinkat(dirfd int, path string, flags int) error {
	var p *byte
	p, err := syscall.BytePtrFromString(path)
	if err != nil {
		return err
	}

	syscall.Entersyscall()
	r := unlinkat(int32(dirfd), p, int32(flags))
	var errno syscall.Errno
	if r < 0 {
		errno = syscall.GetErrno()
	}
	syscall.Exitsyscall()
	if r < 0 {
		return errno
	}
	return nil
}

func Openat(dirfd int, path string, flags int, perm uint32) (int, error) {
	var p *byte
	p, err := syscall.BytePtrFromString(path)
	if err != nil {
		return 0, err
	}

	syscall.Entersyscall()
	fd := openat(int32(dirfd), p, int32(flags), syscall.Mode_t(perm))
	var errno syscall.Errno
	if fd < 0 {
		errno = syscall.GetErrno()
	}
	syscall.Exitsyscall()
	if fd < 0 {
		return 0, errno
	}
	return int(fd), nil
}

func Fstatat(dirfd int, path string, stat *syscall.Stat_t, flags int) error {
	var p *byte
	p, err := syscall.BytePtrFromString(path)
	if err != nil {
		return err
	}

	syscall.Entersyscall()
	r := fstatat(int32(dirfd), p, stat, int32(flags))
	var errno syscall.Errno
	if r < 0 {
		errno = syscall.GetErrno()
	}
	syscall.Exitsyscall()
	if r < 0 {
		return errno
	}

	return nil
}
