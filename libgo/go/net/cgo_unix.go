// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build cgo,!netgo
// +build aix darwin dragonfly freebsd hurd linux netbsd openbsd solaris

package net

/*
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <string.h>

// If nothing else defined EAI_OVERFLOW, make sure it has a value.
#ifndef EAI_OVERFLOW
#define EAI_OVERFLOW -12
#endif
*/

import (
	"context"
	"syscall"
	"unsafe"
)

//extern getaddrinfo
func libc_getaddrinfo(node *byte, service *byte, hints *syscall.Addrinfo, res **syscall.Addrinfo) int32

//extern freeaddrinfo
func libc_freeaddrinfo(res *syscall.Addrinfo)

//extern gai_strerror
func libc_gai_strerror(errcode int) *byte

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

// An addrinfoErrno represents a getaddrinfo, getnameinfo-specific
// error number. It's a signed number and a zero value is a non-error
// by convention.
type addrinfoErrno int

func (eai addrinfoErrno) Error() string   { return bytePtrToString(libc_gai_strerror(int(eai))) }
func (eai addrinfoErrno) Temporary() bool { return eai == syscall.EAI_AGAIN }
func (eai addrinfoErrno) Timeout() bool   { return false }

type portLookupResult struct {
	port int
	err  error
}

type ipLookupResult struct {
	addrs []IPAddr
	cname string
	err   error
}

type reverseLookupResult struct {
	names []string
	err   error
}

func cgoLookupHost(ctx context.Context, name string) (hosts []string, err error, completed bool) {
	addrs, err, completed := cgoLookupIP(ctx, "ip", name)
	for _, addr := range addrs {
		hosts = append(hosts, addr.String())
	}
	return
}

func cgoLookupPort(ctx context.Context, network, service string) (port int, err error, completed bool) {
	var hints syscall.Addrinfo
	switch network {
	case "": // no hints
	case "tcp", "tcp4", "tcp6":
		hints.Ai_socktype = syscall.SOCK_STREAM
		hints.Ai_protocol = syscall.IPPROTO_TCP
	case "udp", "udp4", "udp6":
		hints.Ai_socktype = syscall.SOCK_DGRAM
		hints.Ai_protocol = syscall.IPPROTO_UDP
	default:
		return 0, &DNSError{Err: "unknown network", Name: network + "/" + service}, true
	}
	switch ipVersion(network) {
	case '4':
		hints.Ai_family = syscall.AF_INET
	case '6':
		hints.Ai_family = syscall.AF_INET6
	}
	if ctx.Done() == nil {
		port, err := cgoLookupServicePort(&hints, network, service)
		return port, err, true
	}
	result := make(chan portLookupResult, 1)
	go cgoPortLookup(result, &hints, network, service)
	select {
	case r := <-result:
		return r.port, r.err, true
	case <-ctx.Done():
		// Since there isn't a portable way to cancel the lookup,
		// we just let it finish and write to the buffered channel.
		return 0, mapErr(ctx.Err()), false
	}
}

func cgoLookupServicePort(hints *syscall.Addrinfo, network, service string) (port int, err error) {
	s, err := syscall.BytePtrFromString(service)
	if err != nil {
		return 0, err
	}
	// Lowercase the service name in the memory passed to C.
	for i := 0; i < len(service); i++ {
		bp := (*byte)(unsafe.Pointer(uintptr(unsafe.Pointer(s)) + uintptr(i)))
		*bp = lowerASCII(*bp)
	}
	var res *syscall.Addrinfo
	syscall.Entersyscall()
	gerrno := libc_getaddrinfo(nil, s, hints, &res)
	syscall.Exitsyscall()
	if gerrno != 0 {
		isTemporary := false
		switch gerrno {
		case syscall.EAI_SYSTEM:
			errno := syscall.GetErrno()
			if errno == 0 { // see golang.org/issue/6232
				errno = syscall.EMFILE
			}
			err = errno
		default:
			err = addrinfoErrno(gerrno)
			isTemporary = addrinfoErrno(gerrno).Temporary()
		}
		return 0, &DNSError{Err: err.Error(), Name: network + "/" + service, IsTemporary: isTemporary}
	}
	defer libc_freeaddrinfo(res)

	for r := res; r != nil; r = r.Ai_next {
		switch r.Ai_family {
		case syscall.AF_INET:
			sa := (*syscall.RawSockaddrInet4)(unsafe.Pointer(r.Ai_addr))
			p := (*[2]byte)(unsafe.Pointer(&sa.Port))
			return int(p[0])<<8 | int(p[1]), nil
		case syscall.AF_INET6:
			sa := (*syscall.RawSockaddrInet6)(unsafe.Pointer(r.Ai_addr))
			p := (*[2]byte)(unsafe.Pointer(&sa.Port))
			return int(p[0])<<8 | int(p[1]), nil
		}
	}
	return 0, &DNSError{Err: "unknown port", Name: network + "/" + service}
}

func cgoPortLookup(result chan<- portLookupResult, hints *syscall.Addrinfo, network, service string) {
	port, err := cgoLookupServicePort(hints, network, service)
	result <- portLookupResult{port, err}
}

func cgoLookupIPCNAME(network, name string) (addrs []IPAddr, cname string, err error) {
	acquireThread()
	defer releaseThread()

	var hints syscall.Addrinfo
	hints.Ai_flags = int32(cgoAddrInfoFlags)
	hints.Ai_socktype = syscall.SOCK_STREAM
	hints.Ai_family = syscall.AF_UNSPEC
	switch ipVersion(network) {
	case '4':
		hints.Ai_family = syscall.AF_INET
	case '6':
		hints.Ai_family = syscall.AF_INET6
	}

	h := syscall.StringBytePtr(name)
	var res *syscall.Addrinfo
	syscall.Entersyscall()
	gerrno := libc_getaddrinfo(h, nil, &hints, &res)
	syscall.Exitsyscall()
	if gerrno != 0 {
		isErrorNoSuchHost := false
		isTemporary := false
		switch gerrno {
		case syscall.EAI_SYSTEM:
			errno := syscall.GetErrno()
			if errno == 0 {
				// err should not be nil, but sometimes getaddrinfo returns
				// gerrno == C.EAI_SYSTEM with err == nil on Linux.
				// The report claims that it happens when we have too many
				// open files, so use syscall.EMFILE (too many open files in system).
				// Most system calls would return ENFILE (too many open files),
				// so at the least EMFILE should be easy to recognize if this
				// comes up again. golang.org/issue/6232.
				errno = syscall.EMFILE
			}
			err = errno
		case syscall.EAI_NONAME:
			err = errNoSuchHost
			isErrorNoSuchHost = true
		default:
			err = addrinfoErrno(gerrno)
			isTemporary = addrinfoErrno(gerrno).Temporary()
		}

		return nil, "", &DNSError{Err: err.Error(), Name: name, IsNotFound: isErrorNoSuchHost, IsTemporary: isTemporary}
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
		// We only asked for SOCK_STREAM, but check anyhow.
		if r.Ai_socktype != syscall.SOCK_STREAM {
			continue
		}
		switch r.Ai_family {
		case syscall.AF_INET:
			sa := (*syscall.RawSockaddrInet4)(unsafe.Pointer(r.Ai_addr))
			addr := IPAddr{IP: copyIP(sa.Addr[:])}
			addrs = append(addrs, addr)
		case syscall.AF_INET6:
			sa := (*syscall.RawSockaddrInet6)(unsafe.Pointer(r.Ai_addr))
			addr := IPAddr{IP: copyIP(sa.Addr[:]), Zone: zoneCache.name(int(sa.Scope_id))}
			addrs = append(addrs, addr)
		}
	}
	return addrs, cname, nil
}

func cgoIPLookup(result chan<- ipLookupResult, network, name string) {
	addrs, cname, err := cgoLookupIPCNAME(network, name)
	result <- ipLookupResult{addrs, cname, err}
}

func cgoLookupIP(ctx context.Context, network, name string) (addrs []IPAddr, err error, completed bool) {
	if ctx.Done() == nil {
		addrs, _, err = cgoLookupIPCNAME(network, name)
		return addrs, err, true
	}
	result := make(chan ipLookupResult, 1)
	go cgoIPLookup(result, network, name)
	select {
	case r := <-result:
		return r.addrs, r.err, true
	case <-ctx.Done():
		return nil, mapErr(ctx.Err()), false
	}
}

func cgoLookupCNAME(ctx context.Context, name string) (cname string, err error, completed bool) {
	if ctx.Done() == nil {
		_, cname, err = cgoLookupIPCNAME("ip", name)
		return cname, err, true
	}
	result := make(chan ipLookupResult, 1)
	go cgoIPLookup(result, "ip", name)
	select {
	case r := <-result:
		return r.cname, r.err, true
	case <-ctx.Done():
		return "", mapErr(ctx.Err()), false
	}
}

// These are roughly enough for the following:
//
// Source		Encoding			Maximum length of single name entry
// Unicast DNS		ASCII or			<=253 + a NUL terminator
//			Unicode in RFC 5892		252 * total number of labels + delimiters + a NUL terminator
// Multicast DNS	UTF-8 in RFC 5198 or		<=253 + a NUL terminator
//			the same as unicast DNS ASCII	<=253 + a NUL terminator
// Local database	various				depends on implementation
const (
	nameinfoLen    = 64
	maxNameinfoLen = 4096
)

func cgoLookupPTR(ctx context.Context, addr string) (names []string, err error, completed bool) {
	var zone string
	ip := parseIPv4(addr)
	if ip == nil {
		ip, zone = parseIPv6Zone(addr)
	}
	if ip == nil {
		return nil, &DNSError{Err: "invalid address", Name: addr}, true
	}
	sa, salen := cgoSockaddr(ip, zone)
	if sa == nil {
		return nil, &DNSError{Err: "invalid address " + ip.String(), Name: addr}, true
	}
	if ctx.Done() == nil {
		names, err := cgoLookupAddrPTR(addr, sa, salen)
		return names, err, true
	}
	result := make(chan reverseLookupResult, 1)
	go cgoReverseLookup(result, addr, sa, salen)
	select {
	case r := <-result:
		return r.names, r.err, true
	case <-ctx.Done():
		return nil, mapErr(ctx.Err()), false
	}
}

func cgoLookupAddrPTR(addr string, sa *syscall.RawSockaddr, salen syscall.Socklen_t) (names []string, err error) {
	acquireThread()
	defer releaseThread()

	var gerrno int
	var b []byte
	for l := nameinfoLen; l <= maxNameinfoLen; l *= 2 {
		b = make([]byte, l)
		gerrno, err = cgoNameinfoPTR(b, sa, salen)
		if gerrno == 0 || gerrno != syscall.EAI_OVERFLOW {
			break
		}
	}
	if gerrno != 0 {
		isTemporary := false
		switch gerrno {
		case syscall.EAI_SYSTEM:
			if err == nil { // see golang.org/issue/6232
				err = syscall.EMFILE
			}
		default:
			err = addrinfoErrno(gerrno)
			isTemporary = addrinfoErrno(gerrno).Temporary()
		}
		return nil, &DNSError{Err: err.Error(), Name: addr, IsTemporary: isTemporary}
	}
	for i := 0; i < len(b); i++ {
		if b[i] == 0 {
			b = b[:i]
			break
		}
	}
	return []string{absDomainName(b)}, nil
}

func cgoReverseLookup(result chan<- reverseLookupResult, addr string, sa *syscall.RawSockaddr, salen syscall.Socklen_t) {
	names, err := cgoLookupAddrPTR(addr, sa, salen)
	result <- reverseLookupResult{names, err}
}

func cgoSockaddr(ip IP, zone string) (*syscall.RawSockaddr, syscall.Socklen_t) {
	if ip4 := ip.To4(); ip4 != nil {
		return cgoSockaddrInet4(ip4), syscall.Socklen_t(syscall.SizeofSockaddrInet4)
	}
	if ip6 := ip.To16(); ip6 != nil {
		return cgoSockaddrInet6(ip6, zoneCache.index(zone)), syscall.Socklen_t(syscall.SizeofSockaddrInet6)
	}
	return nil, 0
}

func copyIP(x IP) IP {
	if len(x) < 16 {
		return x.To16()
	}
	y := make(IP, len(x))
	copy(y, x)
	return y
}
