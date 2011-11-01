// socket_linux.go -- Socket handling specific to GNU/Linux.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

const SizeofSockaddrInet4 = 16
const SizeofSockaddrInet6 = 28
const SizeofSockaddrUnix = 110
const SizeofSockaddrLinklayer = 20
const SizeofSockaddrNetlink = 12

type SockaddrLinklayer struct {
	Protocol uint16
	Ifindex  int
	Hatype   uint16
	Pkttype  uint8
	Halen    uint8
	Addr     [8]byte
	raw      RawSockaddrLinklayer
}

func (sa *SockaddrLinklayer) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	if sa.Ifindex < 0 || sa.Ifindex > 0x7fffffff {
		return nil, 0, EINVAL
	}
	sa.raw.Family = AF_PACKET
	sa.raw.Protocol = sa.Protocol
	sa.raw.Ifindex = int32(sa.Ifindex)
	sa.raw.Hatype = sa.Hatype
	sa.raw.Pkttype = sa.Pkttype
	sa.raw.Halen = sa.Halen
	for i := 0; i < len(sa.Addr); i++ {
		sa.raw.Addr[i] = sa.Addr[i]
	}
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), SizeofSockaddrLinklayer, 0
}

type SockaddrNetlink struct {
	Family uint16
	Pad    uint16
	Pid    uint32
	Groups uint32
	raw    RawSockaddrNetlink
}

func (sa *SockaddrNetlink) sockaddr() (*RawSockaddrAny, Socklen_t, int) {
	sa.raw.Family = AF_NETLINK
	sa.raw.Pad = sa.Pad
	sa.raw.Pid = sa.Pid
	sa.raw.Groups = sa.Groups
	return (*RawSockaddrAny)(unsafe.Pointer(&sa.raw)), SizeofSockaddrNetlink, 0
}

type RawSockaddrInet4 struct {
	Family uint16
	Port uint16
	Addr [4]byte /* in_addr */
	Zero [8]uint8
}

func (sa *RawSockaddrInet4) setLen() Socklen_t {
	return SizeofSockaddrInet4
}

type RawSockaddrInet6 struct {
	Family uint16
	Port uint16
	Flowinfo uint32
	Addr [16]byte /* in6_addr */
	Scope_id uint32
}

func (sa *RawSockaddrInet6) setLen() Socklen_t {
	return SizeofSockaddrInet6
}

type RawSockaddrUnix struct {
	Family uint16
	Path [108]int8
}

func (sa *RawSockaddrUnix) setLen(int) {
}

func (sa *RawSockaddrUnix) getLen() (int, int) {
	if sa.Path[0] == 0 {
		// "Abstract" Unix domain socket.
		// Rewrite leading NUL as @ for textual display.
		// (This is the standard convention.)
		// Not friendly to overwrite in place,
		// but the callers below don't care.
		sa.Path[0] = '@'
	}

	// Assume path ends at NUL.
	// This is not technically the GNU/Linux semantics for
	// abstract Unix domain sockets--they are supposed
	// to be uninterpreted fixed-size binary blobs--but
	// everyone uses this convention.
	n := 0
	for n < len(sa.Path) - 3 && sa.Path[n] != 0 {
		n++
	}

	return n, 0
}

type RawSockaddrLinklayer struct {
	Family   uint16
	Protocol uint16
	Ifindex  int32
	Hatype   uint16
	Pkttype  uint8
	Halen    uint8
	Addr     [8]uint8
}

type RawSockaddrNetlink struct {
	Family uint16
	Pad    uint16
	Pid    uint32
	Groups uint32
}

type RawSockaddr struct {
	Family uint16
	Data [14]int8
}

// BindToDevice binds the socket associated with fd to device.
func BindToDevice(fd int, device string) (errno int) {
	return SetsockoptString(fd, SOL_SOCKET, SO_BINDTODEVICE, device)
}

func anyToSockaddrOS(rsa *RawSockaddrAny) (Sockaddr, int) {
	switch rsa.Addr.Family {
	case AF_NETLINK:
		pp := (*RawSockaddrNetlink)(unsafe.Pointer(rsa))
		sa := new(SockaddrNetlink)
		sa.Family = pp.Family
		sa.Pad = pp.Pad
		sa.Pid = pp.Pid
		sa.Groups = pp.Groups
		return sa, 0

	case AF_PACKET:
		pp := (*RawSockaddrLinklayer)(unsafe.Pointer(rsa))
		sa := new(SockaddrLinklayer)
		sa.Protocol = pp.Protocol
		sa.Ifindex = int(pp.Ifindex)
		sa.Hatype = pp.Hatype
		sa.Pkttype = pp.Pkttype
		sa.Halen = pp.Halen
		for i := 0; i < len(sa.Addr); i++ {
			sa.Addr[i] = pp.Addr[i]
		}
		return sa, 0
	}
	return nil, EAFNOSUPPORT
}

//sysnb	EpollCreate(size int) (fd int, errno int)
//epoll_create(size int) int

//sysnb	EpollCtl(epfd int, op int, fd int, event *EpollEvent) (errno int)
//epoll_ctl(epfd int, op int, fd int, event *EpollEvent) int

//sys	EpollWait(epfd int, events []EpollEvent, msec int) (n int, errno int)
//epoll_wait(epfd int, events *EpollEvent, maxevents int, timeout int) int
