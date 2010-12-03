// exec_helpers.go -- helper functions used with fork, exec, wait.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "sync"

// Lock synchronizing creation of new file descriptors with fork.
//
// We want the child in a fork/exec sequence to inherit only the
// file descriptors we intend.  To do that, we mark all file
// descriptors close-on-exec and then, in the child, explicitly
// unmark the ones we want the exec'ed program to keep.
// Unix doesn't make this easy: there is, in general, no way to
// allocate a new file descriptor close-on-exec.  Instead you
// have to allocate the descriptor and then mark it close-on-exec.
// If a fork happens between those two events, the child's exec
// will inherit an unwanted file descriptor.
//
// This lock solves that race: the create new fd/mark close-on-exec
// operation is done holding ForkLock for reading, and the fork itself
// is done holding ForkLock for writing.  At least, that's the idea.
// There are some complications.
//
// Some system calls that create new file descriptors can block
// for arbitrarily long times: open on a hung NFS server or named
// pipe, accept on a socket, and so on.  We can't reasonably grab
// the lock across those operations.
//
// It is worse to inherit some file descriptors than others.
// If a non-malicious child accidentally inherits an open ordinary file,
// that's not a big deal.  On the other hand, if a long-lived child
// accidentally inherits the write end of a pipe, then the reader
// of that pipe will not see EOF until that child exits, potentially
// causing the parent program to hang.  This is a common problem
// in threaded C programs that use popen.
//
// Luckily, the file descriptors that are most important not to
// inherit are not the ones that can take an arbitrarily long time
// to create: pipe returns instantly, and the net package uses
// non-blocking I/O to accept on a listening socket.
// The rules for which file descriptor-creating operations use the
// ForkLock are as follows:
//
// 1) Pipe.    Does not block.  Use the ForkLock.
// 2) Socket.  Does not block.  Use the ForkLock.
// 3) Accept.  If using non-blocking mode, use the ForkLock.
//             Otherwise, live with the race.
// 4) Open.    Can block.  Use O_CLOEXEC if available (Linux).
//             Otherwise, live with the race.
// 5) Dup.     Does not block.  Use the ForkLock.
//             On Linux, could use fcntl F_DUPFD_CLOEXEC
//             instead of the ForkLock, but only for dup(fd, -1).

type WaitStatus int

var ForkLock sync.RWMutex

// Convert array of string to array
// of NUL-terminated byte pointer.
func StringArrayPtr(ss []string) []*byte {
	bb := make([]*byte, len(ss)+1);
	for i := 0; i < len(ss); i++ {
		bb[i] = StringBytePtr(ss[i]);
	}
	bb[len(ss)] = nil;
	return bb;
}

func CloseOnExec(fd int) {
	fcntl(fd, F_SETFD, FD_CLOEXEC);
}

func SetNonblock(fd int, nonblocking bool) (errno int) {
	flag, err := fcntl(fd, F_GETFL, 0);
	if err != 0 {
		return err;
	}
	if nonblocking {
		flag |= O_NONBLOCK;
	} else {
		flag &= ^O_NONBLOCK;
	}
	flag, err = fcntl(fd, F_SETFL, flag);
	return err;
}

// Wait status is 7 bits at bottom, either 0 (exited),
// 0x7F (stopped), or a signal number that caused an exit.
// The 0x80 bit is whether there was a core dump.
// An extra number (exit code, signal causing a stop)
// is in the high bits.  At least that's the idea.
// There are various irregularities.  For example, the
// "continued" status is 0xFFFF, distinguishing itself
// from stopped via the core dump bit.

const (
	mask = 0x7F;
	core = 0x80;
	exited = 0x00;
	stopped = 0x7F;
	shift = 8;
)

func (w WaitStatus) Exited() bool {
	return w&mask == exited;
}

func (w WaitStatus) Signaled() bool {
	return w&mask != stopped && w&mask != exited;
}

func (w WaitStatus) Stopped() bool {
	return w&0xFF == stopped;
}

func (w WaitStatus) Continued() bool {
	return w == 0xFFFF;
}

func (w WaitStatus) CoreDump() bool {
	return w.Signaled() && w&core != 0;
}

func (w WaitStatus) ExitStatus() int {
	if !w.Exited() {
		return -1;
	}
	return int(w >> shift) & 0xFF;
}

func (w WaitStatus) Signal() int {
	if !w.Signaled() {
		return -1;
	}
	return int(w & mask);
}

func (w WaitStatus) StopSignal() int {
	if !w.Stopped() {
		return -1;
	}
	return int(w >> shift) & 0xFF;
}

func (w WaitStatus) TrapCause() int {
	if w.StopSignal() != SIGTRAP {
		return -1;
	}
	return int(w >> shift) >> 8;
}
