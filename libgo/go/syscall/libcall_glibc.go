// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build hurd linux

// glibc library calls.

package syscall

import (
	"internal/race"
	"unsafe"
)

//sys	Openat(dirfd int, path string, flags int, mode uint32) (fd int, err error)
//__go_openat(dirfd _C_int, path *byte, flags _C_int, mode Mode_t) _C_int

//sys	futimesat(dirfd int, path *byte, times *[2]Timeval) (err error)
//futimesat(dirfd _C_int, path *byte, times *[2]Timeval) _C_int
func Futimesat(dirfd int, path string, tv []Timeval) (err error) {
	if len(tv) != 2 {
		return EINVAL
	}
	return futimesat(dirfd, StringBytePtr(path), (*[2]Timeval)(unsafe.Pointer(&tv[0])))
}

func Futimes(fd int, tv []Timeval) (err error) {
	// Believe it or not, this is the best we can do on GNU/Linux
	// (and is what glibc does).
	return Utimes("/proc/self/fd/"+itoa(fd), tv)
}

//sys	ptrace(request int, pid int, addr uintptr, data uintptr) (err error)
//ptrace(request _C_int, pid Pid_t, addr *byte, data *byte) _C_long

//sys	accept4(fd int, sa *RawSockaddrAny, len *Socklen_t, flags int) (nfd int, err error)
//accept4(fd _C_int, sa *RawSockaddrAny, len *Socklen_t, flags _C_int) _C_int

func Accept4(fd int, flags int) (nfd int, sa Sockaddr, err error) {
	var rsa RawSockaddrAny
	var len Socklen_t = SizeofSockaddrAny
	nfd, err = accept4(fd, &rsa, &len, flags)
	if err != nil {
		return -1, nil, err
	}
	sa, err = anyToSockaddr(&rsa)
	if err != nil {
		Close(nfd)
		return -1, nil, err
	}
	return nfd, sa, nil
}

//sysnb	Dup3(oldfd int, newfd int, flags int) (err error)
//dup3(oldfd _C_int, newfd _C_int, flags _C_int) _C_int

//sys	Faccessat(dirfd int, path string, mode uint32, flags int) (err error)
//faccessat(dirfd _C_int, pathname *byte, mode _C_int, flags _C_int) _C_int

//sys	Fallocate(fd int, mode uint32, off int64, len int64) (err error)
//fallocate(fd _C_int, mode _C_int, offset Offset_t, len Offset_t) _C_int

//sys	Fchmodat(dirfd int, path string, mode uint32, flags int) (err error)
//fchmodat(dirfd _C_int, pathname *byte, mode Mode_t, flags _C_int) _C_int

//sys	Fchownat(dirfd int, path string, uid int, gid int, flags int) (err error)
//fchownat(dirfd _C_int, path *byte, owner Uid_t, group Gid_t, flags _C_int) _C_int

//sys	Flock(fd int, how int) (err error)
//flock(fd _C_int, how _C_int) _C_int

func Getdents(fd int, buf []byte) (n int, err error) {
	var p *byte
	if len(buf) > 0 {
		p = &buf[0]
	} else {
		p = (*byte)(unsafe.Pointer(&_zero))
	}
	s := SYS_GETDENTS64
	if s == 0 {
		s = SYS_GETDENTS
	}
	r1, _, errno := Syscall(uintptr(s), uintptr(fd), uintptr(unsafe.Pointer(p)), uintptr(len(buf)))
	n = int(r1)
	if n < 0 {
		err = errno
	}
	return
}

func ReadDirent(fd int, buf []byte) (n int, err error) {
	return Getdents(fd, buf)
}

//sys	Mkdirat(dirfd int, path string, mode uint32) (err error)
//mkdirat(dirfd _C_int, path *byte, mode Mode_t) _C_int

//sys	Mknodat(dirfd int, path string, mode uint32, dev int) (err error)
//mknodat(dirfd _C_int, path *byte, mode Mode_t, dev _dev_t) _C_int

//sysnb	pipe2(p *[2]_C_int, flags int) (err error)
//pipe2(p *[2]_C_int, flags _C_int) _C_int
func Pipe2(p []int, flags int) (err error) {
	if len(p) != 2 {
		return EINVAL
	}
	var pp [2]_C_int
	err = pipe2(&pp, flags)
	p[0] = int(pp[0])
	p[1] = int(pp[1])
	return
}

//sys	sendfile(outfd int, infd int, offset *Offset_t, count int) (written int, err error)
//sendfile64(outfd _C_int, infd _C_int, offset *Offset_t, count Size_t) Ssize_t
func Sendfile(outfd int, infd int, offset *int64, count int) (written int, err error) {
	if race.Enabled {
		race.ReleaseMerge(unsafe.Pointer(&ioSync))
	}
	var soff Offset_t
	var psoff *Offset_t
	if offset != nil {
		soff = Offset_t(*offset)
		psoff = &soff
	}
	written, err = sendfile(outfd, infd, psoff, count)
	if offset != nil {
		*offset = int64(soff)
	}
	return
}

//sys	SyncFileRange(fd int, off int64, n int64, flags int) (err error)
//sync_file_range(fd _C_int, off Offset_t, n Offset_t, flags _C_uint) _C_int
