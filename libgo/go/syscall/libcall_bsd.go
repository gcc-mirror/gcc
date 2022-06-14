// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build darwin || dragonfly || freebsd || netbsd || openbsd
// +build darwin dragonfly freebsd netbsd openbsd

// BSD library calls.

package syscall

import (
	"unsafe"
)

//sys	sysctl(mib []_C_int, old *byte, oldlen *uintptr, new *byte, newlen uintptr) (err error)
//sysctl(mib *_C_int, miblen uintptr, old *byte, oldlen *uintptr, new *byte, newlen uintptr) _C_int

//sysnb raw_ptrace(request int, pid int, addr uintptr, data uintptr) (err Errno)
//ptrace(request _C_int, pid Pid_t, addr *byte, data _C_int) _C_int

//sys	paccept(fd int, rsa *RawSockaddrAny, addrlen *Socklen_t, sigmask *_sigset_t, flags int) (nfd int, err error)
//paccept(s _C_int, rsa *RawSockaddrAny, addrlen *Socklen_t, sigmask *_sigset_t, flags int) _C_int

//sys	Flock(fd int, how int) (err error)
//flock(fd _C_int, how _C_int) _C_int

func ReadDirent(fd int, buf []byte) (n int, err error) {
	// Final argument is (basep *uintptr) and the syscall doesn't take nil.
	// 64 bits should be enough. (32 bits isn't even on 386). Since the
	// actual system call is getdirentries64, 64 is a good guess.
	// TODO(rsc): Can we use a single global basep for all calls?
	var base = (*uintptr)(unsafe.Pointer(new(uint64)))
	return Getdirentries(fd, buf, base)
}

func Accept4(fd, flags int) (nfd int, sa Sockaddr, err error) {
	var rsa RawSockaddrAny
	var len Socklen_t = SizeofSockaddrAny
	nfd, err = paccept(fd, &rsa, &len, nil, flags)
	if err != nil {
		return
	}
	if len > SizeofSockaddrAny {
		panic("RawSockaddrAny too small")
	}
	sa, err = anyToSockaddr(&rsa)
	if err != nil {
		Close(nfd)
		nfd = 0
	}
	return
}

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

func Sysctl(name string) (value string, err error) {
	// Translate name to mib number.
	mib, err := nametomib(name)
	if err != nil {
		return "", err
	}

	// Find size.
	n := uintptr(0)
	if err = sysctl(mib, nil, &n, nil, 0); err != nil {
		return "", err
	}
	if n == 0 {
		return "", nil
	}

	// Read into buffer of that size.
	buf := make([]byte, n)
	if err = sysctl(mib, &buf[0], &n, nil, 0); err != nil {
		return "", err
	}

	// Throw away terminating NUL.
	if n > 0 && buf[n-1] == '\x00' {
		n--
	}
	return string(buf[0:n]), nil
}

func SysctlUint32(name string) (value uint32, err error) {
	// Translate name to mib number.
	mib, err := nametomib(name)
	if err != nil {
		return 0, err
	}

	// Read into buffer of that size.
	n := uintptr(4)
	buf := make([]byte, 4)
	if err = sysctl(mib, &buf[0], &n, nil, 0); err != nil {
		return 0, err
	}
	if n != 4 {
		return 0, EIO
	}
	return *(*uint32)(unsafe.Pointer(&buf[0])), nil
}
