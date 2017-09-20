// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

// This is based on the former libgo/runtime/netpoll_select.c implementation
// except that it uses AIX pollset_poll instead of select and is written in Go.

type pollset_t int32

type pollfd struct {
	fd      int32
	events  int16
	revents int16
}

const _POLLIN = 0x0001
const _POLLOUT = 0x0002
const _POLLHUP = 0x2000
const _POLLERR = 0x4000

type poll_ctl struct {
	cmd    int16
	events int16
	fd     int32
}

const _PS_ADD = 0x0
const _PS_DELETE = 0x2

//extern pollset_create
func pollset_create(maxfd int32) pollset_t

//extern pollset_ctl
func pollset_ctl(ps pollset_t, pollctl_array *poll_ctl, array_length int32) int32

//extern pollset_poll
func pollset_poll(ps pollset_t, polldata_array *pollfd, array_length int32, timeout int32) int32

//extern pipe
func libc_pipe(fd *int32) int32

//extern __go_fcntl_uintptr
func fcntlUintptr(fd, cmd, arg uintptr) (uintptr, uintptr)

func fcntl(fd, cmd int32, arg uintptr) uintptr {
	r, _ := fcntlUintptr(uintptr(fd), uintptr(cmd), arg)
	return r
}

var (
	ps          pollset_t = -1
	mpfds       map[int32]*pollDesc
	pmtx        mutex
	rdwake      int32
	wrwake      int32
	needsUpdate bool
)

func netpollinit() {
	var p [2]int32

	if ps = pollset_create(-1); ps < 0 {
		throw("runtime: netpollinit failed to create pollset")
	}
	// It is not possible to add or remove descriptors from
	// the pollset while pollset_poll is active.
	// We use a pipe to wakeup pollset_poll when the pollset
	// needs to be updated.
	if err := libc_pipe(&p[0]); err < 0 {
		throw("runtime: netpollinit failed to create pipe")
	}
	rdwake = p[0]
	wrwake = p[1]

	fl := fcntl(rdwake, _F_GETFL, 0)
	fcntl(rdwake, _F_SETFL, fl|_O_NONBLOCK)
	fcntl(rdwake, _F_SETFD, _FD_CLOEXEC)

	fl = fcntl(wrwake, _F_GETFL, 0)
	fcntl(wrwake, _F_SETFL, fl|_O_NONBLOCK)
	fcntl(wrwake, _F_SETFD, _FD_CLOEXEC)

	// Add the read side of the pipe to the pollset.
	var pctl poll_ctl
	pctl.cmd = _PS_ADD
	pctl.fd = rdwake
	pctl.events = _POLLIN
	if pollset_ctl(ps, &pctl, 1) != 0 {
		throw("runtime: netpollinit failed to register pipe")
	}

	mpfds = make(map[int32]*pollDesc)
}

func netpolldescriptor() uintptr {
	// ps is not a real file descriptor.
	return ^uintptr(0)
}

func netpollopen(fd uintptr, pd *pollDesc) int32 {
	// pollset_ctl will block if pollset_poll is active
	// so wakeup pollset_poll first.
	lock(&pmtx)
	needsUpdate = true
	unlock(&pmtx)
	b := [1]byte{0}
	write(uintptr(wrwake), unsafe.Pointer(&b[0]), 1)

	var pctl poll_ctl
	pctl.cmd = _PS_ADD
	pctl.fd = int32(fd)
	pctl.events = _POLLIN | _POLLOUT
	if pollset_ctl(ps, &pctl, 1) != 0 {
		return int32(errno())
	}
	lock(&pmtx)
	mpfds[int32(fd)] = pd
	needsUpdate = false
	unlock(&pmtx)

	return 0
}

func netpollclose(fd uintptr) int32 {
	// pollset_ctl will block if pollset_poll is active
	// so wakeup pollset_poll first.
	lock(&pmtx)
	needsUpdate = true
	unlock(&pmtx)
	b := [1]byte{0}
	write(uintptr(wrwake), unsafe.Pointer(&b[0]), 1)

	var pctl poll_ctl
	pctl.cmd = _PS_DELETE
	pctl.fd = int32(fd)
	if pollset_ctl(ps, &pctl, 1) != 0 {
		return int32(errno())
	}
	lock(&pmtx)
	delete(mpfds, int32(fd))
	needsUpdate = false
	unlock(&pmtx)

	return 0
}

func netpollarm(pd *pollDesc, mode int) {
	throw("runtime: unused")
}

func netpoll(block bool) *g {
	if ps == -1 {
		return nil
	}
	timeout := int32(-1)
	if !block {
		timeout = 0
	}
	var pfds [128]pollfd
retry:
	lock(&pmtx)
	if needsUpdate {
		unlock(&pmtx)
		osyield()
		goto retry
	}
	unlock(&pmtx)
	nfound := pollset_poll(ps, &pfds[0], int32(len(pfds)), timeout)
	if nfound < 0 {
		e := errno()
		if e != _EINTR {
			throw("runtime: pollset_poll failed")
		}
		goto retry
	}
	var gp guintptr
	for i := int32(0); i < nfound; i++ {
		pfd := &pfds[i]

		var mode int32
		if pfd.revents&(_POLLIN|_POLLHUP|_POLLERR) != 0 {
			if pfd.fd == rdwake {
				var b [1]byte
				read(pfd.fd, unsafe.Pointer(&b[0]), 1)
				continue
			}
			mode += 'r'
		}
		if pfd.revents&(_POLLOUT|_POLLHUP|_POLLERR) != 0 {
			mode += 'w'
		}
		if mode != 0 {
			lock(&pmtx)
			pd := mpfds[pfd.fd]
			unlock(&pmtx)
			if pd != nil {
				netpollready(&gp, pd, mode)
			}
		}
	}
	if block && gp == 0 {
		goto retry
	}
	return gp.ptr()
}
