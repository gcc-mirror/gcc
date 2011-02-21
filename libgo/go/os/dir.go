// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package os

import (
	"syscall"
	"unsafe"
)

func libc_dup(fd int) int __asm__ ("dup")
func libc_opendir(*byte) *syscall.DIR __asm__ ("opendir")
func libc_closedir(*syscall.DIR) int __asm__ ("closedir")

// FIXME: pathconf returns long, not int.
func libc_pathconf(*byte, int) int __asm__ ("pathconf")

func clen(n []byte) int {
	for i := 0; i < len(n); i++ {
		if n[i] == 0 {
			return i
		}
	}
	return len(n)
}

var elen int;

// Negative count means read until EOF.
func (file *File) Readdirnames(count int) (names []string, err Error) {
	if elen == 0 {
		var dummy syscall.Dirent;
		elen = (unsafe.Offsetof(dummy.Name) +
			libc_pathconf(syscall.StringBytePtr(file.name),	syscall.PC_NAME_MAX) +
			1);
	}

	if file.dirinfo == nil {
		file.dirinfo = new(dirInfo)
		file.dirinfo.buf = make([]byte, elen)
		file.dirinfo.dir = libc_opendir(syscall.StringBytePtr(file.name))
	}

	entry_dirent := unsafe.Pointer(&file.dirinfo.buf[0]).(*syscall.Dirent)

	size := count
	if size < 0 {
		size = 100
	}
	names = make([]string, 0, size) // Empty with room to grow.

	dir := file.dirinfo.dir
	if dir == nil {
		return names, NewSyscallError("opendir", syscall.GetErrno())
	}	

	for count != 0 {
		var result *syscall.Dirent
		i := libc_readdir_r(dir, entry_dirent, &result)
		if result == nil {
			break
		}
		var name = string(result.Name[0:clen(result.Name[0:])])
		if name == "." || name == ".." {	// Useless names
			continue
		}
		count--
		names = append(names, name)
	}
	return names, nil
}
