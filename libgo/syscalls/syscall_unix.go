// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

var (
	Stdin  = 0
	Stdout = 1
	Stderr = 2
)

const ENONE = 0

func GetErrno() int
func SetErrno(int)

func Uname(buf *Utsname) (errno int) {
	r := libc_uname(buf)
	if r < 0 {
		errno = GetErrno()
	}
	return
}

var mapper = &mmapper{
	active: make(map[*byte][]byte),
	mmap:   mmap,
	munmap: munmap,
}

func Mmap(fd int, offset int64, length int, prot int, flags int) (data []byte, errno int) {
	return mapper.Mmap(fd, offset, length, prot, flags)
}

func Munmap(b []byte) (errno int) {
	return mapper.Munmap(b)
}

func libc_munmap(*byte, Size_t) int __asm__ ("munmap")

func mmap(addr uintptr, length uintptr, prot int, flag int, fd int, pos int64) (ret uintptr, errno int) {
	r0 := libc_mmap((*byte)(unsafe.Pointer(addr)), Size_t(length), prot, flag, fd, Offset_t(pos))
	ret = uintptr(unsafe.Pointer(r0))
	if ret + 1 == 0 {
		errno = GetErrno()
	}
	return
}

func munmap(addr uintptr, length uintptr) (errno int) {
	if libc_munmap((*byte)(unsafe.Pointer(addr)), Size_t(length)) < 0 {
		errno = GetErrno()
	}
	return
}

func libc_getrusage(who int, rusage *Rusage) int __asm__ ("getrusage")

func Getrusage(who int, rusage *Rusage) (errno int) {
	if libc_getrusage(who, rusage) < 0 {
		errno = GetErrno()
	}
	return
}
