// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build cgo,!netgo
// +build aix darwin dragonfly freebsd hurd netbsd openbsd

package net

/*
#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>
*/

import (
	"syscall"
	"unsafe"
)

func cgoSockaddrInet4(ip IP) *syscall.RawSockaddr {
	sa := syscall.RawSockaddrInet4{Len: syscall.SizeofSockaddrInet4, Family: syscall.AF_INET}
	copy(sa.Addr[:], ip)
	return (*syscall.RawSockaddr)(unsafe.Pointer(&sa))
}

func cgoSockaddrInet6(ip IP, zone int) *syscall.RawSockaddr {
	sa := syscall.RawSockaddrInet6{Len: syscall.SizeofSockaddrInet6, Family: syscall.AF_INET6, Scope_id: uint32(zone)}
	copy(sa.Addr[:], ip)
	return (*syscall.RawSockaddr)(unsafe.Pointer(&sa))
}
