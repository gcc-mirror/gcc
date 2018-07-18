// socket_aix.go -- Socket handling specific to AIX.

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

const SizeofSockaddrInet4 = 16
const SizeofSockaddrInet6 = 28
const SizeofSockaddrUnix = 1025

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
	Path   [1023]int8
}

func (sa *RawSockaddrUnix) setLen(n int) {
	sa.Len = uint8(3 + n) // 2 for Family, Len; 1 for NUL.
}

func (sa *RawSockaddrUnix) getLen() (int, error) {
	// Some versions of AIX have a bug in getsockname (see IV78655).
	// We can't rely on sa.Len being set correctly.
	n := SizeofSockaddrUnix - 3 // substract leading Family, Len, terminating NUL.
	for i := 0; i < n; i++ {
		if sa.Path[i] == 0 {
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
