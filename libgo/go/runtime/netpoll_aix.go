// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

// This is based on the former libgo/runtime/netpoll_select.c implementation
// except that it uses poll instead of select and is written in Go.

// These definitions should come from sysinfo.go as they may be OS-dependent.
// These are the definitions for the AIX operating system.
type pollfd struct {
	fd      int32
	events  int16
	revents int16
}

const _POLLIN = 0x0001
const _POLLOUT = 0x0002
const _POLLHUP = 0x2000
const _POLLERR = 0x4000

//extern poll
func libc_poll(pfds *pollfd, npfds uintptr, timeout uintptr) int32

//extern pipe
func libc_pipe(fd *int32) int32

//extern __go_fcntl_uintptr
func fcntlUintptr(fd, cmd, arg uintptr) (uintptr, uintptr)

func closeonexec(fd int32) {
	fcntlUintptr(uintptr(fd), _F_SETFD, _FD_CLOEXEC)
}

var (
	allocated int
	pfds      []pollfd
	mpfds     map[uintptr]*pollDesc
	pmtx      mutex
	rdwake    int32
	wrwake    int32
)

func netpollinit() {
	var p [2]int32

	// Create the pipe we use to wakeup poll.
	if err := libc_pipe(&p[0]); err < 0 {
		throw("netpollinit: failed to create pipe")
	}
	rdwake = p[0]
	wrwake = p[1]

	closeonexec(rdwake)
	closeonexec(wrwake)

	// Pre-allocate array of pollfd structures for poll.
	allocated = 128
	pfds = make([]pollfd, allocated)

	mpfds = make(map[uintptr]*pollDesc)
}

func netpollopen(fd uintptr, pd *pollDesc) int32 {
	lock(&pmtx)
	mpfds[fd] = pd
	unlock(&pmtx)

	// Wakeup poll.
	b := [1]byte{0}
	write(uintptr(wrwake), unsafe.Pointer(&b[0]), 1)

	return 0
}

func netpollclose(fd uintptr) int32 {
	lock(&pmtx)
	delete(mpfds, fd)
	unlock(&pmtx)

	// Wakeup poll.
	b := [1]byte{0}
	write(uintptr(wrwake), unsafe.Pointer(&b[0]), 1)

	return 0
}

func netpollarm(pd *pollDesc, mode int) {
	throw("unused")
}

func netpoll(block bool) *g {
	if allocated == 0 {
		return nil
	}
	timeout := ^uintptr(0)
	if !block {
		timeout = 0
	}
retry:
	lock(&pmtx)
	npfds := len(mpfds) + 1
	unlock(&pmtx)

	if npfds > allocated {
		for npfds > allocated {
			allocated *= 2
		}
		pfds = make([]pollfd, allocated)
	}

	// Poll the read side of the pipe.
	pfds[0].fd = rdwake
	pfds[0].events = _POLLIN
	lock(&pmtx)
	// Notice that npfds may have changed since we released the lock.
	// Just copy what we can, new descriptors will be added at next
	// iteration.
	i := 1
	for fd := range mpfds {
		if i >= allocated {
			break
		}
		pfds[i].fd = int32(fd)
		pfds[i].events = _POLLIN | _POLLOUT
		i++
	}
	npfds = i
	unlock(&pmtx)

	n := libc_poll(&pfds[0], uintptr(npfds), timeout)
	if n < 0 {
		e := errno()
		if e != _EINTR {
			throw("poll failed")
		}
		goto retry
	}
	var gp guintptr
	for i = 0; i < npfds && n > 0; i++ {
		pfd := pfds[i]

		var mode int32
		if pfd.revents&(_POLLIN|_POLLHUP|_POLLERR) != 0 {
			if i == 0 {
				var b [1]byte
				read(pfd.fd, unsafe.Pointer(&b[0]), 1)
				n--
				continue
			}
			mode += 'r'
		}
		if pfd.revents&(_POLLOUT|_POLLHUP|_POLLERR) != 0 {
			mode += 'w'
		}
		if mode != 0 {
			lock(&pmtx)
			pd := mpfds[uintptr(pfd.fd)]
			unlock(&pmtx)
			if pd != nil {
				netpollready(&gp, pd, mode)
			}
			n--
		}
	}
	if block && gp == 0 {
		goto retry
	}
	return gp.ptr()
}
