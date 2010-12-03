// socket_linux.go -- Socket handling specific to Linux.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Low-level socket interface.
// Only for implementing net package.

// DO NOT USE DIRECTLY.

package syscall

import "unsafe"

type RawSockaddrInet4 struct {
	Family uint16;
	Port uint16;
	Addr [4]byte /* in_addr */;
	Zero [8]uint8;
}

type RawSockaddrInet6 struct {
	Family uint16;
	Port uint16;
	Flowinfo uint32;
	Addr [16]byte /* in6_addr */;
	Scope_id uint32;
}

type RawSockaddrUnix struct {
	Family uint16;
	Path [108]int8;
}

type RawSockaddr struct {
	Family uint16;
	Data [14]int8;
}

func (sa *SockaddrInet4) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	if sa.Port < 0 || sa.Port > 0xFFFF {
		return nil, 0, EINVAL;
	}
	sa.raw.Family = AF_INET;
	p := (*[2]byte)(unsafe.Pointer(&sa.raw.Port));
	p[0] = byte(sa.Port>>8);
	p[1] = byte(sa.Port);
	for i := 0; i < len(sa.Addr); i++ {
		sa.raw.Addr[i] = sa.Addr[i];
	}
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), SizeofSockaddrInet4, 0;
}

func (sa *SockaddrInet6) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	if sa.Port < 0 || sa.Port > 0xFFFF {
		return nil, 0, EINVAL;
	}
	sa.raw.Family = AF_INET6;
	p := (*[2]byte)(unsafe.Pointer(&sa.raw.Port));
	p[0] = byte(sa.Port>>8);
	p[1] = byte(sa.Port);
	for i := 0; i < len(sa.Addr); i++ {
		sa.raw.Addr[i] = sa.Addr[i];
	}
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), SizeofSockaddrInet6, 0;
}

func (sa *SockaddrUnix) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	name := sa.Name;
	n := len(name);
	if n >= len(sa.raw.Path) || n == 0 {
		return nil, 0, EINVAL;
	}
	sa.raw.Family = AF_UNIX;
	for i := 0; i < n; i++ {
		sa.raw.Path[i] = int8(name[i]);
	}
	if sa.raw.Path[0] == '@' {
		sa.raw.Path[0] = 0;
	}

	// length is family, name, NUL.
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), 1 + Socklen_t(n) + 1, 0;
}

func anyToSockaddr(rsa *RawSockaddrAny) (Sockaddr, int) {
	switch rsa.Addr.Family {
	case AF_UNIX:
		pp := (*RawSockaddrUnix)(unsafe.Pointer(rsa));
		sa := new(SockaddrUnix);
		if pp.Path[0] == 0 {
			// "Abstract" Unix domain socket.
			// Rewrite leading NUL as @ for textual display.
			// (This is the standard convention.)
			// Not friendly to overwrite in place,
			// but the callers below don't care.
			pp.Path[0] = '@';
		}

		// Assume path ends at NUL.
		// This is not technically the Linux semantics for
		// abstract Unix domain sockets--they are supposed
		// to be uninterpreted fixed-size binary blobs--but
		// everyone uses this convention.
		n := 0;
		for n < len(pp.Path) - 3 && pp.Path[n] != 0 {
			n++;
		}
		bytes := (*[len(pp.Path)]byte)(unsafe.Pointer(&pp.Path[0]));
		sa.Name = string(bytes[0:n]);
		return sa, 0;

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
	return SetsockoptString(fd, SOL_SOCKET, SO_BINDTODEVICE, device)
}
