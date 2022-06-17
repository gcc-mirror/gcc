// socket_hurd.go -- Socket handling specific to GNU/Hurd based systems.
// This file is derived from socket_bsd.go without SockaddrDatalink since
// AF_LINK is not yet supported on GNU/Hurd.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build hurd
// +build hurd

package syscall

import "unsafe"

const SizeofSockaddrInet4 = 16
const SizeofSockaddrInet6 = 28
const SizeofSockaddrUnix = 110

type RawSockaddrInet4 struct {
	Len    uint8
	Family uint8
	Port   uint16
	Addr   [4]byte /* in_addr */
	Zero   [8]uint8
}

func (sa *RawSockaddrInet4) setLen() Socklen_t {
	sa.Len = SizeofSockaddrInet4
	return SizeofSockaddrInet4
}

type RawSockaddrInet6 struct {
	Len      uint8
	Family   uint8
	Port     uint16
	Flowinfo uint32
	Addr     [16]byte /* in6_addr */
	Scope_id uint32
}

func (sa *RawSockaddrInet6) setLen() Socklen_t {
	sa.Len = SizeofSockaddrInet6
	return SizeofSockaddrInet6
}

type RawSockaddrUnix struct {
	Len    uint8
	Family uint8
	Path   [108]int8
}

func (sa *RawSockaddrUnix) setLen(n int) {
	sa.Len = uint8(3 + n) // 2 for Family, Len; 1 for NUL.
}

func (sa *RawSockaddrUnix) getLen() (int, error) {
	if sa.Len < 2 || sa.Len > SizeofSockaddrUnix {
		return 0, EINVAL
	}

	// Some BSDs include the trailing NUL in the length, whereas
	// others do not. Work around this by subtracting the leading
	// family and len. The path is then scanned to see if a NUL
	// terminator still exists within the length.
	n := int(sa.Len) - 2 // subtract leading Family, Len
	for i := 0; i < n; i++ {
		if sa.Path[i] == 0 {
			// found early NUL; assume Len included the NUL
			// or was overestimating.
			n = i
			break
		}
	}
	return n, nil
}

func (sa *RawSockaddrUnix) adjustAbstract(sl Socklen_t) Socklen_t {
	return sl
}

type RawSockaddr struct {
	Len    uint8
	Family uint8
	Data   [14]int8
}

// BindToDevice binds the socket associated with fd to device.
func BindToDevice(fd int, device string) (err error) {
	return ENOSYS
}

func anyToSockaddrOS(rsa *RawSockaddrAny) (Sockaddr, error) {
	return nil, EAFNOSUPPORT
}

func GetsockoptIPv6MTUInfo(fd, level, opt int) (*IPv6MTUInfo, error) {
	var value IPv6MTUInfo
	vallen := Socklen_t(SizeofIPv6MTUInfo)
	err := getsockopt(fd, level, opt, unsafe.Pointer(&value), &vallen)
	return &value, err
}
