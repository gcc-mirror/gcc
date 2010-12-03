// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Waiting for FDs via select(2).

package net

import (
	"os"
	"syscall"
)

type pollster struct {
	readFds, writeFds, repeatFds *syscall.FdSet_t
	maxFd int
	readyReadFds, readyWriteFds *syscall.FdSet_t
	nReady int
	lastFd int
}

func newpollster() (p *pollster, err os.Error) {
	p = new(pollster)
	p.readFds = new(syscall.FdSet_t)
	p.writeFds = new(syscall.FdSet_t)
	p.repeatFds = new(syscall.FdSet_t)
	p.readyReadFds = new(syscall.FdSet_t)
	p.readyWriteFds = new(syscall.FdSet_t)
	p.maxFd = -1
	p.nReady = 0
	p.lastFd = 0
	return p, nil
}

func (p *pollster) AddFD(fd int, mode int, repeat bool) os.Error {
	if mode == 'r' {
		syscall.FDSet(fd, p.readFds)
	} else {
		syscall.FDSet(fd, p.writeFds)
	}

	if repeat {
		syscall.FDSet(fd, p.repeatFds)
	}

	if fd > p.maxFd {
		p.maxFd = fd
	}

	return nil
}

func (p *pollster) DelFD(fd int, mode int) {
	if mode == 'r' {
		if !syscall.FDIsSet(fd, p.readFds) {
			print("Select unexpected fd=", fd, " for read\n")
			return
		}
		syscall.FDClr(fd, p.readFds)
	} else {
		if !syscall.FDIsSet(fd, p.writeFds) {
			print("Select unexpected fd=", fd, " for write\n")
			return
		}
		syscall.FDClr(fd, p.writeFds)
	}

	// Doesn't matter if not already present.
	syscall.FDClr(fd, p.repeatFds)

	// We don't worry about maxFd here.
}

func (p *pollster) WaitFD(nsec int64) (fd int, mode int, err os.Error) {
	if p.nReady == 0 {
		var timeout *syscall.Timeval
		var tv syscall.Timeval
		timeout = nil
		if nsec > 0 {
			tv = syscall.NsecToTimeval(nsec)
			timeout = &tv
		}

		var n, e int
		var tmpReadFds, tmpWriteFds syscall.FdSet_t
		for {
			// Temporary syscall.FdSet_ts into which the values are copied
			// because select mutates the values.
			tmpReadFds = *p.readFds
			tmpWriteFds = *p.writeFds

			n, e = syscall.Select(p.maxFd + 1, &tmpReadFds, &tmpWriteFds, nil, timeout)
			if e != syscall.EINTR {
				break
			}
		}
		if e != 0 {
			return -1, 0, os.NewSyscallError("select", e)
		}
		if n == 0 {
			return -1, 0, nil
		}

		p.nReady = n
		*p.readyReadFds = tmpReadFds
		*p.readyWriteFds = tmpWriteFds
		p.lastFd = 0
	}

	flag := false
	for i := p.lastFd; i < p.maxFd + 1; i++ {
		if syscall.FDIsSet(i, p.readyReadFds) {
			flag = true
			mode = 'r'
			syscall.FDClr(i, p.readyReadFds)
		} else if syscall.FDIsSet(i, p.readyWriteFds) {
			flag = true
			mode = 'w'
			syscall.FDClr(i, p.readyWriteFds)
		}
		if flag {
			if !syscall.FDIsSet(i, p.repeatFds) {
				p.DelFD(i, mode)
			}
			p.nReady--
			p.lastFd = i
			return i, mode, nil
		}
	}

	// Will not reach here.  Just to shut up the compiler.
	return -1, 0, nil
}

func (p *pollster) Close() os.Error {
	return nil
}
