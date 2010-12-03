// socket_bsd.go -- Socket handling specific to *BSD based systems.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Low-level socket interface.
// Only for implementing net package.
// DO NOT USE DIRECTLY.

package syscall

import "unsafe"

type RawSockaddrInet4 struct {
	Len uint8;
	Family uint8;
	Port uint16;
	Addr [4]byte /* in_addr */;
	Zero [8]uint8;
}

type RawSockaddrInet6 struct {
	Len uint8;
	Family uint8;
	Port uint16;
	Flowinfo uint32;
	Addr [16]byte /* in6_addr */;
	Scope_id uint32;
}

type RawSockaddrUnix struct {
	Len uint8;
	Family uint8;
	Path [108]int8;
}

type RawSockaddr struct {
	Len uint8;
	Family uint8;
	Data [14]int8;
}

func (sa *SockaddrInet4) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	if sa.Port < 0 || sa.Port > 0xFFFF {
		return nil, 0, EINVAL;
	}
	sa.raw.Len = SizeofSockaddrInet4;
	sa.raw.Family = AF_INET;
	p := (*[2]byte)(unsafe.Pointer(&sa.raw.Port));
	p[0] = byte(sa.Port>>8);
	p[1] = byte(sa.Port);
	for i := 0; i < len(sa.Addr); i++ {
		sa.raw.Addr[i] = sa.Addr[i];
	}
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), Socklen_t(sa.raw.Len), 0;
}

func (sa *SockaddrInet6) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	if sa.Port < 0 || sa.Port > 0xFFFF {
		return nil, 0, EINVAL;
	}
	sa.raw.Len = SizeofSockaddrInet6;
	sa.raw.Family = AF_INET6;
	p := (*[2]byte)(unsafe.Pointer(&sa.raw.Port));
	p[0] = byte(sa.Port>>8);
	p[1] = byte(sa.Port);
	for i := 0; i < len(sa.Addr); i++ {
		sa.raw.Addr[i] = sa.Addr[i];
	}
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), Socklen_t(sa.raw.Len), 0;
}

func (sa *SockaddrUnix) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	name := sa.Name;
	n := len(name);
	if n >= len(sa.raw.Path) || n == 0 {
		return nil, 0, EINVAL;
	}
	sa.raw.Len = byte(3 + n); // 2 for Family, Len; 1 for NUL.
	sa.raw.Family = AF_UNIX;
	for i := 0; i < n; i++ {
		sa.raw.Path[i] = int8(name[i]);
	}
	if sa.raw.Path[0] == '@' {
		sa.raw.Path[0] = 0;
	}

	// length is family, name, NUL.
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), Socklen_t(sa.raw.Len), 0;
}

func anyToSockaddr(rsa *RawSockaddrAny) (Sockaddr, int) {
	switch rsa.Addr.Family {
	case AF_UNIX:
		pp := (*RawSockaddrUnix)(unsafe.Pointer(rsa))
		if pp.Len < 3 || pp.Len > SizeofSockaddrUnix {
			return nil, EINVAL
		}
		sa := new(SockaddrUnix)
		n := int(pp.Len) - 3 // subtract leading Family, Len, terminating NUL.
		for i := 0; i < n; i++ {
			if pp.Path[i] == 0 {
				// found early NUL; assume Len is overestimating.
				n = i
				break
			}
		}
		bytes := (*[len(pp.Path)]byte)(unsafe.Pointer(&pp.Path[0]))
		sa.Name = string(bytes[0:n])
		return sa, 0

	case AF_INET:
		pp := (*RawSockaddrInet4)(unsafe.Pointer(rsa));
		sa := new(SockaddrInet4);
		p := (*[2]byte)(unsafe.Pointer(&pp.Port));
		sa.Port = int(p[0])<<8 + int(p[1]);
		for i := 0; i < len(sa.Addr); i++ {
			sa.Addr[i] = pp.Addr[i];
		}
		return sa, 0;

	case AF_INET6:
		pp := (*RawSockaddrInet6)(unsafe.Pointer(rsa));
		sa := new(SockaddrInet6);
		p := (*[2]byte)(unsafe.Pointer(&pp.Port));
		sa.Port = int(p[0])<<8 + int(p[1]);
		for i := 0; i < len(sa.Addr); i++ {
			sa.Addr[i] = pp.Addr[i];
		}
		return sa, 0;
	}
	return nil, EAFNOSUPPORT;
}

// BindToDevice binds the socket associated with fd to device.
func BindToDevice(fd int, device string) (errno int) {
	return ENOSYS
}
