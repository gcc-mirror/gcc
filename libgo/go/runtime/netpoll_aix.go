// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

// This is based on the former libgo/runtime/netpoll_select.c implementation
// except that it uses poll instead of select and is written in Go.

type pollfd struct {
	fd      int32
	events  int16
	revents int16
}

const _POLLIN = 0x0001
const _POLLOUT = 0x0002
const _POLLHUP = 0x2000
const _POLLERR = 0x4000

//go:noescape
//extern poll
func libc_poll(pfds *pollfd, npfds uintptr, timeout uintptr) int32

//go:noescape
//extern pipe
func libc_pipe(fd *int32) int32

//extern __go_fcntl_uintptr
func fcntlUintptr(fd, cmd, arg uintptr) (uintptr, uintptr)

func fcntl(fd, cmd int32, arg uintptr) uintptr {
	r, _ := fcntlUintptr(uintptr(fd), uintptr(cmd), arg)
	return r
}

var (
	pfds        []pollfd
	pds         []*pollDesc
	mtxpoll     mutex
	mtxset      mutex
	rdwake      int32
	wrwake      int32
	needsUpdate bool
)

func netpollinit() {
	var p [2]int32

	// Create the pipe we use to wakeup poll.
	if err := libc_pipe(&p[0]); err < 0 {
		throw("runtime: netpollinit failed to create pipe")
	}
	rdwake = p[0]
	wrwake = p[1]

	fl := fcntl(rdwake, _F_GETFL, 0)
	fcntl(rdwake, _F_SETFL, fl|_O_NONBLOCK)
	fcntl(rdwake, _F_SETFD, _FD_CLOEXEC)

	fl = fcntl(wrwake, _F_GETFL, 0)
	fcntl(wrwake, _F_SETFD, _FD_CLOEXEC)

	// Pre-allocate array of pollfd structures for poll.
	pfds = make([]pollfd, 1, 128)
	// Poll the read side of the pipe.
	pfds[0].fd = rdwake
	pfds[0].events = _POLLIN

	// Allocate index to pd array
	pds = make([]*pollDesc, 1, 128)
	pds[0] = nil
}

func netpolldescriptor() uintptr {
	return ^uintptr(0)
}

func netpollwakeup() {
	if !needsUpdate {
		needsUpdate = true
		b := [1]byte{0}
		write(uintptr(wrwake), unsafe.Pointer(&b[0]), 1)
	}
}

func netpollopen(fd uintptr, pd *pollDesc) int32 {
	lock(&mtxpoll)
	netpollwakeup()

	lock(&mtxset)
	unlock(&mtxpoll)

	pd.user = uint32(len(pfds))
	var pfd pollfd
	pfd.fd = int32(fd)
	pfds = append(pfds, pfd)
	pds = append(pds, pd)
	unlock(&mtxset)
	return 0
}

func netpollclose(fd uintptr) int32 {
	lock(&mtxpoll)
	netpollwakeup()

	lock(&mtxset)
	unlock(&mtxpoll)

	for i := 0; i < len(pfds); i++ {
		if pfds[i].fd == int32(fd) {
			pfds[i] = pfds[len(pfds)-1]
			pfds = pfds[:len(pfds)-1]

			pds[i] = pds[len(pds)-1]
			pds[i].user = uint32(i)
			pds = pds[:len(pds)-1]
			break
		}
	}
	unlock(&mtxset)
	return 0
}

func netpollarm(pd *pollDesc, mode int) {
	lock(&mtxpoll)
	netpollwakeup()

	lock(&mtxset)
	unlock(&mtxpoll)

	switch mode {
	case 'r':
		pfds[pd.user].events |= _POLLIN
	case 'w':
		pfds[pd.user].events |= _POLLOUT
	}
	unlock(&mtxset)
}

//go:nowritebarrierrec
func netpoll(block bool) *g {
	timeout := ^uintptr(0)
	if !block {
		timeout = 0
		return nil
	}
retry:
	lock(&mtxpoll)
	lock(&mtxset)
	needsUpdate = false
	unlock(&mtxpoll)

	n := libc_poll(&pfds[0], uintptr(len(pfds)), timeout)
	if n < 0 {
		e := errno()
		if e != _EINTR {
			throw("runtime: poll failed")
		}
		unlock(&mtxset)
		goto retry
	}
	// Check if some descriptors need to be changed
	if n != 0 && pfds[0].revents&(_POLLIN|_POLLHUP|_POLLERR) != 0 {
		var b [1]byte
		for read(rdwake, unsafe.Pointer(&b[0]), 1) == 1 {
		}
		// Do not look at the other fds in this case as the mode may have changed
		// XXX only additions of flags are made, so maybe it is ok
		unlock(&mtxset)
		goto retry
	}
	var gp guintptr
	for i := 0; i < len(pfds) && n > 0; i++ {
		pfd := &pfds[i]

		var mode int32
		if pfd.revents&(_POLLIN|_POLLHUP|_POLLERR) != 0 {
			mode += 'r'
			pfd.events &= ^_POLLIN
		}
		if pfd.revents&(_POLLOUT|_POLLHUP|_POLLERR) != 0 {
			mode += 'w'
			pfd.events &= ^_POLLOUT
		}
		if mode != 0 {
			netpollready(&gp, pds[i], mode)
			n--
		}
	}
	unlock(&mtxset)
	if block && gp == 0 {
		goto retry
	}
	return gp.ptr()
}
