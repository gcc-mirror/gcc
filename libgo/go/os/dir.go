// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package os

import (
	"io"
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

// Readdirnames reads and returns a slice of names from the directory f.
//
// If n > 0, Readdirnames returns at most n names. In this case, if
// Readdirnames returns an empty slice, it will return a non-nil error
// explaining why. At the end of a directory, the error is os.EOF.
//
// If n <= 0, Readdirnames returns all the names from the directory in
// a single slice. In this case, if Readdirnames succeeds (reads all
// the way to the end of the directory), it returns the slice and a
// nil error. If it encounters an error before the end of the
// directory, Readdirnames returns the names read until that point and
// a non-nil error.
func (file *File) Readdirnames(n int) (names []string, err error) {
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

	size := n
	if size < 0 {
		size = 100
		n = -1
	}

	names = make([]string, 0, size) // Empty with room to grow.

	dir := file.dirinfo.dir
	if dir == nil {
		return names, NewSyscallError("opendir", syscall.GetErrno())
	}	

	for n != 0 {
		var result *syscall.Dirent
		i := libc_readdir_r(dir, entry_dirent, &result)
		if i != 0 {
			return names, NewSyscallError("readdir_r", i)
		}
		if result == nil {
			break // EOF
		}
		var name = string(result.Name[0:clen(result.Name[0:])])
		if name == "." || name == ".." {	// Useless names
			continue
		}
		names = append(names, name)
		n--
	}
	if n >= 0 && len(names) == 0 {
		return names, io.EOF
	}
	return names, nil
}
