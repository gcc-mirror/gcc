// socket_solaris.go -- Socket handling specific to Solaris.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

const SizeofSockaddrInet4 = 16
const SizeofSockaddrInet6 = 32
const SizeofSockaddrUnix = 110

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
	Src_id uint32
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

type RawSockaddr struct {
	Family uint16
	Data [14]int8
}

// BindToDevice binds the socket associated with fd to device.
func BindToDevice(fd int, device string) (errno int) {
	return ENOSYS
}

func anyToSockaddrOS(rsa *RawSockaddrAny) (Sockaddr, int) {
	return nil, EAFNOSUPPORT;
}
