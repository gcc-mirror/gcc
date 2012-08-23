// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package os

import (
	"io"
	"syscall"
	"unsafe"
)

//extern opendir
func libc_opendir(*byte) *syscall.DIR

//extern closedir
func libc_closedir(*syscall.DIR) int

// FIXME: pathconf returns long, not int.
//extern pathconf
func libc_pathconf(*byte, int) int

func clen(n []byte) int {
	for i := 0; i < len(n); i++ {
		if n[i] == 0 {
			return i
		}
	}
	return len(n)
}

var elen int

func (file *File) readdirnames(n int) (names []string, err error) {
	if elen == 0 {
		var dummy syscall.Dirent
		elen = (int(unsafe.Offsetof(dummy.Name)) +
			libc_pathconf(syscall.StringBytePtr(file.name), syscall.PC_NAME_MAX) +
			1)
	}

	if file.dirinfo == nil {
		file.dirinfo = new(dirInfo)
		file.dirinfo.buf = make([]byte, elen)
		p := syscall.StringBytePtr(file.name)
		syscall.Entersyscall()
		r := libc_opendir(p)
		syscall.Exitsyscall()
		file.dirinfo.dir = r
	}

	entry_dirent := (*syscall.Dirent)(unsafe.Pointer(&file.dirinfo.buf[0]))

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
		pr := &result
		syscall.Entersyscall()
		i := libc_readdir_r(dir, entry_dirent, pr)
		syscall.Exitsyscall()
		if i != 0 {
			return names, NewSyscallError("readdir_r", i)
		}
		if result == nil {
			break // EOF
		}
		var name = string(result.Name[0:clen(result.Name[0:])])
		if name == "." || name == ".." { // Useless names
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
