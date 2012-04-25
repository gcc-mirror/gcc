// Copyright 2012 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build solaris

// Sockets for Solaris

package net

import (
	"syscall"
)

func maxListenerBacklog() int {
	// The kernel does not track the limit.
	return syscall.SOMAXCONN
}

func listenerSockaddr(s, f int, la syscall.Sockaddr, toAddr func(syscall.Sockaddr) Addr) (syscall.Sockaddr, error) {
	a := toAddr(la)
	if a == nil {
		return la, nil
	}
	switch v := a.(type) {
	case *TCPAddr, *UnixAddr:
		err := setDefaultListenerSockopts(s)
		if err != nil {
			return nil, err
		}
	case *UDPAddr:
		if v.IP.IsMulticast() {
			err := setDefaultMulticastSockopts(s)
			if err != nil {
				return nil, err
			}
			switch f {
			case syscall.AF_INET:
				v.IP = IPv4zero
			case syscall.AF_INET6:
				v.IP = IPv6unspecified
			}
			return v.sockaddr(f)
		}
	}
	return la, nil
}
