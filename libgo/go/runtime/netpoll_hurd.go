// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

// FIXME: Improve network poller for hurd.
// This is based on the former libgo/runtime/netpoll_select.c implementation
// except that it uses poll instead of select and is written in Go.
// It's also based on Solaris implementation for the arming mechanisms
// Inspiration was also taken from netpoll_aix.go and netpoll_solaris.go

//From /usr/include/x86_64-linux-gnu/sys/poll.h
//go:noescape
//extern poll
func libc_poll(pfds *pollfd, nfds int32, timeout int32) int32

//go:noescape
//extern pipe2
func libc_pipe2(fd *int32, flags int32) int32

//pollfd represents the poll structure for GNU/Hurd operating system.
type pollfd struct {
	fd      int32 // File descriptor to poll.
	events  int16 // Types of events poller cares about.
	revents int16 // Types of events that actually occurred.
}

//From /usr/include/i386-gnu/bits/poll.h
const _POLLIN = 01    // There is data to read.
const _POLLPRI = 02   // There is urgent data to read.
const _POLLOUT = 04   // Writing now will not block.
const _POLLERR = 010  // Error condition.
const _POLLHUP = 020  // Hung up.
const _POLLNVAL = 040 // Invalid polling request.

var (
	pfds           []pollfd
	pds            []*pollDesc
	mtxpoll        mutex
	mtxset         mutex
	rdwake         int32
	wrwake         int32
	pendingUpdates int32
)

const pollVerbose = false

func netpollinit() {
	var p [2]int32

	// Create the pipe we use to wakeup poll.
	if err := libc_pipe2(&p[0], _O_CLOEXEC|_O_NONBLOCK); err < 0 {
		throw("runtime:netpollinit(): failed to create pipe2")
	}
	rdwake = p[0]
	wrwake = p[1]

	// Pre-allocate array of pollfd structures for poll.
	if pollVerbose {
		println("*** allocating")
	}
	pfds = make([]pollfd, 1, 128)
	if pollVerbose {
		println("*** allocating done", &pfds[0])
	}

	// Poll the read side of the pipe.
	pfds[0].fd = int32(rdwake)
	pfds[0].events = int16(_POLLIN)
	pfds[0].revents = int16(0)

	pds = make([]*pollDesc, 1, 128)
	// Checks for pd != nil are made in netpoll.
	pds[0] = nil
}

func netpolldescriptor() uintptr {
	// Both fds must be returned.
	if rdwake > 0xFFFF || wrwake > 0xFFFF {
		throw("netpolldescriptor: invalid fd number")
	}
	return uintptr(rdwake<<16 | wrwake)
}

// netpollwakeup writes on wrwake to wakeup poll before any changes.
func netpollwakeup() {
	if pendingUpdates == 0 {
		pendingUpdates = 1
		if pollVerbose {
			println("*** writing 1 byte")
		}
		b := [1]byte{0}
		write(uintptr(wrwake), unsafe.Pointer(&b[0]), 1)
	}
}

func netpollopen(fd uintptr, pd *pollDesc) int32 {
	if pollVerbose {
		println("*** netpollopen", fd)
	}
	lock(&mtxpoll)
	netpollwakeup()

	lock(&mtxset)
	unlock(&mtxpoll)

	pd.user = uint32(len(pfds))
	pfds = append(pfds, pollfd{fd: int32(fd)})
	pds = append(pds, pd)
	unlock(&mtxset)
	return 0
}

func netpollclose(fd uintptr) int32 {
	if pollVerbose {
		println("*** netpollclose", fd)
	}
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
	if pollVerbose {
		println("*** netpollarm", pd.fd, mode)
	}
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

// polls for ready network connections
// returns list of goroutines that become runnable
//go:nowritebarrierrec
func netpoll(block bool) gList {
	timeout := int32(0)
	if !block {
		timeout = 0
		return gList{}
	}
	if pollVerbose {
		println("*** netpoll", block)
	}
retry:
	lock(&mtxpoll)
	lock(&mtxset)
	pendingUpdates = 0
	unlock(&mtxpoll)

	if pollVerbose {
		println("*** netpoll before poll")
	}
	n := libc_poll(&pfds[0], int32(len(pfds)), timeout)
	if pollVerbose {
		println("*** netpoll after poll", n)
	}
	if n < 0 {
		e := errno()
		if e != _EINTR {
			println("errno=", e, " len(pfds)=", len(pfds))
			throw("poll failed")
		}
		if pollVerbose {
			println("*** poll failed")
		}
		unlock(&mtxset)
		goto retry
	}
	// Check if some descriptors need to be changed
	if n != 0 && pfds[0].revents&(_POLLIN|_POLLHUP|_POLLERR) != 0 {
		var b [1]byte
		for read(rdwake, unsafe.Pointer(&b[0]), 1) == 1 {
			if pollVerbose {
				println("*** read 1 byte from pipe")
			}
		}
		// Do not look at the other fds in this case as the mode may have changed
		// XXX only additions of flags are made, so maybe it is ok
		unlock(&mtxset)
		goto retry
	}
	var toRun gList
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
			if pollVerbose {
				println("*** netpollready i=", i, "revents=", pfd.revents, "events=", pfd.events, "pd=", pds[i])
			}
			netpollready(&toRun, pds[i], mode)
			n--
		}
	}
	unlock(&mtxset)
	if block && toRun.empty() {
		goto retry
	}
	if pollVerbose {
		println("*** netpoll returning end")
	}
	return toRun
}
