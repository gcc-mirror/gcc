// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin freebsd linux

package net

/*
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
*/

import (
	"os"
	"syscall"
	"unsafe"
)

func libc_getaddrinfo(node *byte, service *byte, hints *syscall.Addrinfo, res **syscall.Addrinfo) int __asm__ ("getaddrinfo")
func libc_freeaddrinfo(res *syscall.Addrinfo) __asm__ ("freeaddrinfo")
func libc_gai_strerror(errcode int) *byte __asm__ ("gai_strerror")

// bytePtrToString takes a NUL-terminated array of bytes and convert
// it to a Go string.
func bytePtrToString(p *byte) string {
	a := (*[10000]byte)(unsafe.Pointer(p))
	i := 0
	for a[i] != 0 {
		i++
	}
	return string(a[:i])
}

func cgoLookupHost(name string) (addrs []string, err os.Error, completed bool) {
	ip, err, completed := cgoLookupIP(name)
	for _, p := range ip {
		addrs = append(addrs, p.String())
	}
	return
}

func cgoLookupPort(net, service string) (port int, err os.Error, completed bool) {
	var res *syscall.Addrinfo
	var hints syscall.Addrinfo

	switch net {
	case "":
		// no hints
	case "tcp", "tcp4", "tcp6":
		hints.Ai_socktype = syscall.SOCK_STREAM
		hints.Ai_protocol = syscall.IPPROTO_TCP
	case "udp", "udp4", "udp6":
		hints.Ai_socktype = syscall.SOCK_DGRAM
		hints.Ai_protocol = syscall.IPPROTO_UDP
	default:
		return 0, UnknownNetworkError(net), true
	}
	if len(net) >= 4 {
		switch net[3] {
		case '4':
			hints.Ai_family = syscall.AF_INET
		case '6':
			hints.Ai_family = syscall.AF_INET6
		}
	}

	s := syscall.StringBytePtr(service)
	if libc_getaddrinfo(nil, s, &hints, &res) == 0 {
		defer libc_freeaddrinfo(res)
		for r := res; r != nil; r = r.Ai_next {
			switch r.Ai_family {
			default:
				continue
			case syscall.AF_INET:
				sa := (*syscall.RawSockaddrInet4)(unsafe.Pointer(r.Ai_addr))
				p := (*[2]byte)(unsafe.Pointer(&sa.Port))
				return int(p[0])<<8 | int(p[1]), nil, true
			case syscall.AF_INET6:
				sa := (*syscall.RawSockaddrInet6)(unsafe.Pointer(r.Ai_addr))
				p := (*[2]byte)(unsafe.Pointer(&sa.Port))
				return int(p[0])<<8 | int(p[1]), nil, true
			}
		}
	}
	return 0, &AddrError{"unknown port", net + "/" + service}, true
}

func cgoLookupIPCNAME(name string) (addrs []IP, cname string, err os.Error, completed bool) {
	var res *syscall.Addrinfo
	var hints syscall.Addrinfo

	// NOTE(rsc): In theory there are approximately balanced
	// arguments for and against including AI_ADDRCONFIG
	// in the flags (it includes IPv4 results only on IPv4 systems,
	// and similarly for IPv6), but in practice setting it causes
	// getaddrinfo to return the wrong canonical name on Linux.
	// So definitely leave it out.
	hints.Ai_flags = int32((syscall.AI_ALL | syscall.AI_V4MAPPED | syscall.AI_CANONNAME) & cgoAddrInfoMask())

	h := syscall.StringBytePtr(name)
	gerrno := libc_getaddrinfo(h, nil, &hints, &res)
	if gerrno != 0 {
		var str string
		if gerrno == syscall.EAI_NONAME {
			str = noSuchHost
		} else if gerrno == syscall.EAI_SYSTEM {
			str = syscall.Errstr(syscall.GetErrno())
		} else {
			str = bytePtrToString(libc_gai_strerror(gerrno))
		}
		return nil, "", &DNSError{Error: str, Name: name}, true
	}
	defer libc_freeaddrinfo(res)
	if res != nil {
		cname = bytePtrToString((*byte)(unsafe.Pointer(res.Ai_canonname)))
		if cname == "" {
			cname = name
		}
		if len(cname) > 0 && cname[len(cname)-1] != '.' {
			cname += "."
		}
	}
	for r := res; r != nil; r = r.Ai_next {
		// Everything comes back twice, once for UDP and once for TCP.
		if r.Ai_socktype != syscall.SOCK_STREAM {
			continue
		}
		switch r.Ai_family {
		default:
			continue
		case syscall.AF_INET:
			sa := (*syscall.RawSockaddrInet4)(unsafe.Pointer(r.Ai_addr))
			addrs = append(addrs, copyIP(sa.Addr[:]))
		case syscall.AF_INET6:
			sa := (*syscall.RawSockaddrInet6)(unsafe.Pointer(r.Ai_addr))
			addrs = append(addrs, copyIP(sa.Addr[:]))
		}
	}
	return addrs, cname, nil, true
}

func cgoLookupIP(name string) (addrs []IP, err os.Error, completed bool) {
	addrs, _, err, completed = cgoLookupIPCNAME(name)
	return
}

func cgoLookupCNAME(name string) (cname string, err os.Error, completed bool) {
	_, cname, err, completed = cgoLookupIPCNAME(name)
	return
}

func copyIP(x IP) IP {
	y := make(IP, len(x))
	copy(y, x)
	return y
}
