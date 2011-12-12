// socket.go -- Socket handling.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Low-level socket interface.
// Only for implementing net package.
// DO NOT USE DIRECTLY.

package syscall

import "unsafe"

// For testing: clients can set this flag to force
// creation of IPv6 sockets to return EAFNOSUPPORT.
var SocketDisableIPv6 bool

type Sockaddr interface {
	sockaddr() (ptr *RawSockaddrAny, len Socklen_t, err error)	// lowercase; only we can define Sockaddrs
}

type RawSockaddrAny struct {
	Addr RawSockaddr
	Pad [12]int8
}

const SizeofSockaddrAny = 0x1c

type SockaddrInet4 struct {
	Port int
	Addr [4]byte
	raw RawSockaddrInet4
}

func (sa *SockaddrInet4) sockaddr() (*RawSockaddrAny, Socklen_t, error) {
	if sa.Port < 0 || sa.Port > 0xFFFF {
		return nil, 0, EINVAL
	}
	sa.raw.Family = AF_INET
	n := sa.raw.setLen()
	p := (*[2]byte)(unsafe.Pointer(&sa.raw.Port))
	p[0] = byte(sa.Port>>8)
	p[1] = byte(sa.Port)
	for i := 0; i < len(sa.Addr); i++ {
		sa.raw.Addr[i] = sa.Addr[i]
	}
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), n, nil
}

type SockaddrInet6 struct {
	Port int
	ZoneId uint32
	Addr [16]byte
	raw RawSockaddrInet6
}

func (sa *SockaddrInet6) sockaddr() (*RawSockaddrAny, Socklen_t, error) {
	if sa.Port < 0 || sa.Port > 0xFFFF {
		return nil, 0, EINVAL
	}
	sa.raw.Family = AF_INET6
	n := sa.raw.setLen()
	p := (*[2]byte)(unsafe.Pointer(&sa.raw.Port))
	p[0] = byte(sa.Port>>8)
	p[1] = byte(sa.Port)
	sa.raw.Scope_id = sa.ZoneId
	for i := 0; i < len(sa.Addr); i++ {
		sa.raw.Addr[i] = sa.Addr[i]
	}
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), n, nil
}

type SockaddrUnix struct {
	Name string
	raw RawSockaddrUnix
}

func (sa *SockaddrUnix) sockaddr() (*RawSockaddrAny, Socklen_t, error) {
	name := sa.Name
	n := len(name)
	if n >= len(sa.raw.Path) || n == 0 {
		return nil, 0, EINVAL
	}
	sa.raw.Family = AF_UNIX
	sa.raw.setLen(n)
	for i := 0; i < n; i++ {
		sa.raw.Path[i] = int8(name[i])
	}
	if sa.raw.Path[0] == '@' {
		sa.raw.Path[0] = 0
	}

	// length is family (uint16), name, NUL.
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), 2 + Socklen_t(n) + 1, nil
}

func anyToSockaddr(rsa *RawSockaddrAny) (Sockaddr, error) {
	switch rsa.Addr.Family {
	case AF_UNIX:
		pp := (*RawSockaddrUnix)(unsafe.Pointer(rsa))
		sa := new(SockaddrUnix)
		n, err := pp.getLen()
		if err != nil {
			return nil, err
		}
		bytes := (*[len(pp.Path)]byte)(unsafe.Pointer(&pp.Path[0]))
		sa.Name = string(bytes[0:n])
		return sa, nil

	case AF_INET:
		pp := (*RawSockaddrInet4)(unsafe.Pointer(rsa))
		sa := new(SockaddrInet4)
		p := (*[2]byte)(unsafe.Pointer(&pp.Port))
		sa.Port = int(p[0])<<8 + int(p[1])
		for i := 0; i < len(sa.Addr); i++ {
			sa.Addr[i] = pp.Addr[i]
		}
		return sa, nil

	case AF_INET6:
		pp := (*RawSockaddrInet6)(unsafe.Pointer(rsa))
		sa := new(SockaddrInet6)
		p := (*[2]byte)(unsafe.Pointer(&pp.Port))
		sa.Port = int(p[0])<<8 + int(p[1])
		for i := 0; i < len(sa.Addr); i++ {
			sa.Addr[i] = pp.Addr[i]
		}
		return sa, nil
	}
	return anyToSockaddrOS(rsa)
}

//sys	accept(fd int, sa *RawSockaddrAny, len *Socklen_t) (nfd int, err error)
//accept(fd int, sa *RawSockaddrAny, len *Socklen_t) int

func Accept(fd int) (nfd int, sa Sockaddr, err error) {
	var rsa RawSockaddrAny
	var len Socklen_t = SizeofSockaddrAny
	nfd, err = accept(fd, &rsa, &len)
	if err != nil {
		return
	}
	sa, err = anyToSockaddr(&rsa)
	if err != nil {
		Close(nfd)
		nfd = 0
	}
	return
}

//sysnb	getsockname(fd int, sa *RawSockaddrAny, len *Socklen_t) (err error)
//getsockname(fd int, sa *RawSockaddrAny, len *Socklen_t) int

func Getsockname(fd int) (sa Sockaddr, err error) {
	var rsa RawSockaddrAny
	var len Socklen_t = SizeofSockaddrAny
	if err = getsockname(fd, &rsa, &len); err != nil {
		return
	}
	return anyToSockaddr(&rsa)
}

//sysnb getpeername(fd int, sa *RawSockaddrAny, len *Socklen_t) (err error)
//getpeername(fd int, sa *RawSockaddrAny, len *Socklen_t) int

func Getpeername(fd int) (sa Sockaddr, err error) {
	var rsa RawSockaddrAny
	var len Socklen_t = SizeofSockaddrAny
	if err = getpeername(fd, &rsa, &len); err != nil {
		return
	}
	return anyToSockaddr(&rsa)
}

//sys	bind(fd int, sa *RawSockaddrAny, len Socklen_t) (err error)
//bind(fd int, sa *RawSockaddrAny, len Socklen_t) int

func Bind(fd int, sa Sockaddr) (err error) {
	ptr, n, err := sa.sockaddr()
	if err != nil {
		return err
	}
	return bind(fd, ptr, n)
}

//sys	connect(s int, addr *RawSockaddrAny, addrlen Socklen_t) (err error)
//connect(s int, addr *RawSockaddrAny, addrlen Socklen_t) int

func Connect(fd int, sa Sockaddr) (err error) {
	ptr, n, err := sa.sockaddr()
	if err != nil {
		return err
	}
	return connect(fd, ptr, n)
}

//sysnb	socket(domain int, typ int, proto int) (fd int, err error)
//socket(domain int, typ int, protocol int) int

func Socket(domain, typ, proto int) (fd int, err error) {
	if domain == AF_INET6 && SocketDisableIPv6 {
		return -1, EAFNOSUPPORT
	}
	fd, err = socket(domain, typ, proto)
	return
}

//sysnb	socketpair(domain int, typ int, proto int, fd *[2]int) (err error)
//socketpair(domain int, typ int, protocol int, fd *[2]int) int

func Socketpair(domain, typ, proto int) (fd [2]int, err error) {
	err = socketpair(domain, typ, proto, &fd)
	return
}

//sys	getsockopt(s int, level int, name int, val uintptr, vallen *Socklen_t) (err error)
//getsockopt(s int, level int, name int, val *byte, vallen *Socklen_t) int

func GetsockoptInt(fd, level, opt int) (value int, err error) {
	var n int32
	vallen := Socklen_t(4)
	err = getsockopt(fd, level, opt, (uintptr)(unsafe.Pointer(&n)), &vallen)
	return int(n), err
}

func GetsockoptInet4Addr(fd, level, opt int) (value [4]byte, err error) {
	vallen := Socklen_t(4)
	err = getsockopt(fd, level, opt, uintptr(unsafe.Pointer(&value[0])), &vallen)
	return value, err
}

func GetsockoptIPMreq(fd, level, opt int) (*IPMreq, error) {
	var value IPMreq
	vallen := Socklen_t(SizeofIPMreq)
	err := getsockopt(fd, level, opt, uintptr(unsafe.Pointer(&value)), &vallen)
	return &value, err
}

/* FIXME: mksysinfo needs to support IPMreqn.

func GetsockoptIPMreqn(fd, level, opt int) (*IPMreqn, error) {
	var value IPMreqn
	vallen := Socklen_t(SizeofIPMreqn)
	err := getsockopt(fd, level, opt, uintptr(unsafe.Pointer(&value)), &vallen)
	return &value, err
}

*/

/* FIXME: mksysinfo needs to support IPv6Mreq.

func GetsockoptIPv6Mreq(fd, level, opt int) (*IPv6Mreq, error) {
	var value IPv6Mreq
	vallen := Socklen_t(SizeofIPv6Mreq)
	err := getsockopt(fd, level, opt, uintptr(unsafe.Pointer(&value)), &vallen)
	return &value, err
}

*/

//sys	setsockopt(s int, level int, name int, val *byte, vallen Socklen_t) (err error)
//setsockopt(s int, level int, optname int, val *byte, vallen Socklen_t) int

func SetsockoptInt(fd, level, opt int, value int) (err error) {
	var n = int32(value)
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(&n)), 4)
}

func SetsockoptInet4Addr(fd, level, opt int, value [4]byte) (err error) {
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(&value[0])), 4)
}

func SetsockoptTimeval(fd, level, opt int, tv *Timeval) (err error) {
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(tv)), Socklen_t(unsafe.Sizeof(*tv)))
}

type Linger struct {
	Onoff int32;
	Linger int32;
}

func SetsockoptLinger(fd, level, opt int, l *Linger) (err error) {
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(l)), Socklen_t(unsafe.Sizeof(*l)));
}

func SetsockoptIPMreq(fd, level, opt int, mreq *IPMreq) (err error) {
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(mreq)), Socklen_t(unsafe.Sizeof(*mreq)))
}

/* FIXME: mksysinfo needs to support IMPreqn.

func SetsockoptIPMreqn(fd, level, opt int, mreq *IPMreqn) (err error) {
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(mreq)), Socklen_t(unsafe.Sizeof(*mreq)))
}

*/

func SetsockoptIPv6Mreq(fd, level, opt int, mreq *IPv6Mreq) (err error) {
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(mreq)), Socklen_t(unsafe.Sizeof(*mreq)))
}

func SetsockoptString(fd, level, opt int, s string) (err error) {
	return setsockopt(fd, level, opt, (*byte)(unsafe.Pointer(&[]byte(s)[0])), Socklen_t(len(s)))
}

//sys	recvfrom(fd int, p []byte, flags int, from *RawSockaddrAny, fromlen *Socklen_t) (n int, err error)
//recvfrom(fd int, buf *byte, len Size_t, flags int, from *RawSockaddrAny, fromlen *Socklen_t) Ssize_t

func Recvfrom(fd int, p []byte, flags int) (n int, from Sockaddr, err error) {
	var rsa RawSockaddrAny
	var len Socklen_t = SizeofSockaddrAny
	if n, err = recvfrom(fd, p, flags, &rsa, &len); err != nil {
		return
	}
	from, err = anyToSockaddr(&rsa)
	return
}

//sys	sendto(s int, buf []byte, flags int, to *RawSockaddrAny, tolen Socklen_t) (err error)
//sendto(s int, buf *byte, len Size_t, flags int, to *RawSockaddrAny, tolen Socklen_t) Ssize_t

func Sendto(fd int, p []byte, flags int, to Sockaddr) (err error) {
	ptr, n, err := to.sockaddr()
	if err != nil {
		return err
	}
	return sendto(fd, p, flags, ptr, n)
}

//sys	recvmsg(s int, msg *Msghdr, flags int) (n int, err error)
//recvmsg(s int, msg *Msghdr, flags int) Ssize_t

func Recvmsg(fd int, p, oob []byte, flags int) (n, oobn int, recvflags int, from Sockaddr, err error) {
	var msg Msghdr
	var rsa RawSockaddrAny
	msg.Name = (*byte)(unsafe.Pointer(&rsa))
	msg.Namelen = uint32(SizeofSockaddrAny)
	var iov Iovec
	if len(p) > 0 {
		iov.Base = (*byte)(unsafe.Pointer(&p[0]))
		iov.SetLen(len(p))
	}
	var dummy byte
	if len(oob) > 0 {
		// receive at least one normal byte
		if len(p) == 0 {
			iov.Base = &dummy
			iov.SetLen(1)
		}
		msg.Control = (*byte)(unsafe.Pointer(&oob[0]))
		msg.SetControllen(len(oob))
	}
	msg.Iov = &iov
	msg.Iovlen = 1
	if n, err = recvmsg(fd, &msg, flags); err != nil {
		return
	}
	oobn = int(msg.Controllen)
	recvflags = int(msg.Flags)
	// source address is only specified if the socket is unconnected
	if rsa.Addr.Family != AF_UNSPEC {
		from, err = anyToSockaddr(&rsa)
	}
	return
}

//sys	sendmsg(s int, msg *Msghdr, flags int) (err error)
//sendmsg(s int, msg *Msghdr, flags int) Ssize_t

func Sendmsg(fd int, p, oob []byte, to Sockaddr, flags int) (err error) {
	var ptr *RawSockaddrAny
	var salen Socklen_t
	if to != nil {
		var err error
		ptr, salen, err = to.sockaddr()
		if err != nil {
			return err
		}
	}
	var msg Msghdr
	msg.Name = (*byte)(unsafe.Pointer(ptr))
	msg.Namelen = uint32(salen)
	var iov Iovec
	if len(p) > 0 {
		iov.Base = (*byte)(unsafe.Pointer(&p[0]))
		iov.SetLen(len(p))
	}
	var dummy byte
	if len(oob) > 0 {
		// send at least one normal byte
		if len(p) == 0 {
			iov.Base = &dummy
			iov.SetLen(1)
		}
		msg.Control = (*byte)(unsafe.Pointer(&oob[0]))
		msg.SetControllen(len(oob))
	}
	msg.Iov = &iov
	msg.Iovlen = 1
	if err = sendmsg(fd, &msg, flags); err != nil {
		return
	}
	return
}

//sys	Listen(fd int, n int) (err error)
//listen(fd int, n int) int

//sys	Shutdown(fd int, how int) (err error)
//shutdown(fd int, how int) int

func (iov *Iovec) SetLen(length int) {
	iov.Len = Iovec_len_t(length)
}

func (msghdr *Msghdr) SetControllen(length int) {
	msghdr.Controllen = Msghdr_controllen_t(length)
}
