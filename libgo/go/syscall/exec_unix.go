// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Fork, exec, wait, etc.

package syscall

import (
	"sync"
	"unsafe"
)

//sysnb	raw_fork() (pid Pid_t, errno int)
//fork() Pid_t

//sysnb raw_setsid() (errno int)
//setsid() Pid_t

//sysnb	raw_chroot(path *byte) (errno int)
//chroot(path *byte) int

//sysnb	raw_chdir(path *byte) (errno int)
//chdir(path *byte) int

//sysnb	raw_fcntl(fd int, cmd int, arg int) (val int, errno int)
//fcntl(fd int, cmd int, arg int) int

//sysnb	raw_close(fd int) (errno int)
//close(fd int) int

//sysnb	raw_ioctl(fd int, cmd int, val int) (rval int, errno int)
//ioctl(fd int, cmd int, val int) int

//sysnb	raw_execve(argv0 *byte, argv **byte, envv **byte) (errno int)
//execve(argv0 *byte, argv **byte, envv **byte) int

//sysnb	raw_read(fd int, p *byte, np int) (n int, errno int)
//read(fd int, buf *byte, count Size_t) Ssize_t

//sysnb	raw_write(fd int, buf *byte, count int) int
//write(fd int, buf *byte, count Size_t) Ssize_t

//sysnb	raw_exit(status int)
//_exit(status int)

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
// 4) Open.    Can block.  Use O_CLOEXEC if available (GNU/Linux).
//             Otherwise, live with the race.
// 5) Dup.     Does not block.  Use the ForkLock.
//             On GNU/Linux, could use fcntl F_DUPFD_CLOEXEC
//             instead of the ForkLock, but only for dup(fd, -1).

var ForkLock sync.RWMutex

// Convert array of string to array
// of NUL-terminated byte pointer.
func StringSlicePtr(ss []string) []*byte {
	bb := make([]*byte, len(ss)+1)
	for i := 0; i < len(ss); i++ {
		bb[i] = StringBytePtr(ss[i])
	}
	bb[len(ss)] = nil
	return bb
}

func CloseOnExec(fd int) { fcntl(fd, F_SETFD, FD_CLOEXEC) }

func SetNonblock(fd int, nonblocking bool) (errno int) {
	flag, err := fcntl(fd, F_GETFL, 0)
	if err != 0 {
		return err
	}
	if nonblocking {
		flag |= O_NONBLOCK
	} else {
		flag &= ^O_NONBLOCK
	}
	_, err = fcntl(fd, F_SETFL, flag)
	return err
}

// Fork, dup fd onto 0..len(fd), and exec(argv0, argvv, envv) in child.
// If a dup or exec fails, write the errno int to pipe.
// (Pipe is close-on-exec so if exec succeeds, it will be closed.)
// In the child, this function must not acquire any locks, because
// they might have been locked at the time of the fork.  This means
// no rescheduling, no malloc calls, and no new stack segments.
// The calls to RawSyscall are okay because they are assembly
// functions that do not grow the stack.
func forkAndExecInChild(argv0 *byte, argv, envv []*byte, chroot, dir *byte, attr *ProcAttr, sys *SysProcAttr, pipe int) (pid int, err int) {
	// Declare all variables at top in case any
	// declarations require heap allocation (e.g., err1).
	var r1 Pid_t
	var err1 int
	var nextfd int
	var i int

	// guard against side effects of shuffling fds below.
	fd := append([]int(nil), attr.Files...)

	// About to call fork.
	// No more allocation or calls of non-assembly functions.
	r1, err1 = raw_fork()
	if err1 != 0 {
		return 0, int(err1)
	}

	if r1 != 0 {
		// parent; return PID
		return int(r1), 0
	}

	// Fork succeeded, now in child.

	// Enable tracing if requested.
	if sys.Ptrace {
		err1 = raw_ptrace(_PTRACE_TRACEME, 0, nil, nil)
		if err1 != 0 {
			goto childerror
		}
	}

	// Session ID
	if sys.Setsid {
		err1 = raw_setsid()
		if err1 != 0 {
			goto childerror
		}
	}

	// Set process group
	if sys.Setpgid {
		err1 = Setpgid(0, 0)
		if err1 != 0 {
			goto childerror
		}
	}

	// Chroot
	if chroot != nil {
		err1 = raw_chroot(chroot)
		if err1 != 0 {
			goto childerror
		}
	}

	// User and groups
	if cred := sys.Credential; cred != nil {
		ngroups := len(cred.Groups)
		if ngroups == 0 {
			err1 = setgroups(0, nil)
		} else {
			groups := make([]Gid_t, ngroups)
			for i, v := range cred.Groups {
				groups[i] = Gid_t(v)
			}
			err1 = setgroups(ngroups, &groups[0])
		}
		if err1 != 0 {
			goto childerror
		}
		err1 = Setgid(int(cred.Gid))
		if err1 != 0 {
			goto childerror
		}
		err1 = Setuid(int(cred.Uid))
		if err1 != 0 {
			goto childerror
		}
	}

	// Chdir
	if dir != nil {
		err1 = raw_chdir(dir)
		if err1 != 0 {
			goto childerror
		}
	}

	// Pass 1: look for fd[i] < i and move those up above len(fd)
	// so that pass 2 won't stomp on an fd it needs later.
	nextfd = int(len(fd))
	if pipe < nextfd {
		_, err1 = Dup2(pipe, nextfd)
		if err1 != 0 {
			goto childerror
		}
		raw_fcntl(nextfd, F_SETFD, FD_CLOEXEC)
		pipe = nextfd
		nextfd++
	}
	for i = 0; i < len(fd); i++ {
		if fd[i] >= 0 && fd[i] < int(i) {
			_, err1 = Dup2(fd[i], nextfd)
			if err1 != 0 {
				goto childerror
			}
			raw_fcntl(nextfd, F_SETFD, FD_CLOEXEC)
			fd[i] = nextfd
			nextfd++
			if nextfd == pipe { // don't stomp on pipe
				nextfd++
			}
		}
	}

	// Pass 2: dup fd[i] down onto i.
	for i = 0; i < len(fd); i++ {
		if fd[i] == -1 {
			raw_close(i)
			continue
		}
		if fd[i] == int(i) {
			// Dup2(i, i) won't clear close-on-exec flag on
			// GNU/Linux, probably not elsewhere either.
			_, err1 = raw_fcntl(fd[i], F_SETFD, 0)
			if err1 != 0 {
				goto childerror
			}
			continue
		}
		// The new fd is created NOT close-on-exec,
		// which is exactly what we want.
		_, err1 = Dup2(fd[i], i)
		if err1 != 0 {
			goto childerror
		}
	}

	// By convention, we don't close-on-exec the fds we are
	// started with, so if len(fd) < 3, close 0, 1, 2 as needed.
	// Programs that know they inherit fds >= 3 will need
	// to set them close-on-exec.
	for i = len(fd); i < 3; i++ {
		raw_close(i)
	}

	// Detach fd 0 from tty
	if sys.Noctty {
		_, err1 = raw_ioctl(0, TIOCNOTTY, 0)
		if err1 != 0 {
			goto childerror
		}
	}

	// Make fd 0 the tty
	if sys.Setctty {
		_, err1 = raw_ioctl(0, TIOCSCTTY, 0)
		if err1 != 0 {
			goto childerror
		}
	}

	// Time to exec.
	err1 = raw_execve(argv0, &argv[0], &envv[0])

childerror:
	// send error code on pipe
	raw_write(pipe, (*byte)(unsafe.Pointer(&err1)), int(unsafe.Sizeof(err1)))
	for {
		raw_exit(253)
	}

	// Calling panic is not actually safe,
	// but the for loop above won't break
	// and this shuts up the compiler.
	panic("unreached")
}

// Credential holds user and group identities to be assumed
// by a child process started by StartProcess.
type Credential struct {
	Uid    uint32   // User ID.
	Gid    uint32   // Group ID.
	Groups []uint32 // Supplementary group IDs.
}

// ProcAttr holds attributes that will be applied to a new process started
// by StartProcess.
type ProcAttr struct {
	Dir   string   // Current working directory.
	Env   []string // Environment.
	Files []int    // File descriptors.
	Sys   *SysProcAttr
}

type SysProcAttr struct {
	Chroot     string      // Chroot.
	Credential *Credential // Credential.
	Ptrace     bool        // Enable tracing.
	Setsid     bool        // Create session.
	Setpgid    bool        // Set process group ID to new pid (SYSV setpgrp)
	Setctty    bool        // Set controlling terminal to fd 0
	Noctty     bool        // Detach fd 0 from controlling terminal
}

var zeroProcAttr ProcAttr
var zeroSysProcAttr SysProcAttr

func forkExec(argv0 string, argv []string, attr *ProcAttr) (pid int, err int) {
	var p [2]int
	var n int
	var err1 uintptr
	var wstatus WaitStatus

	if attr == nil {
		attr = &zeroProcAttr
	}
	sys := attr.Sys
	if sys == nil {
		sys = &zeroSysProcAttr
	}

	p[0] = -1
	p[1] = -1

	// Convert args to C form.
	argv0p := StringBytePtr(argv0)
	argvp := StringSlicePtr(argv)
	envvp := StringSlicePtr(attr.Env)

	if OS == "freebsd" && len(argv[0]) > len(argv0) {
		argvp[0] = argv0p
	}

	var chroot *byte
	if sys.Chroot != "" {
		chroot = StringBytePtr(sys.Chroot)
	}
	var dir *byte
	if attr.Dir != "" {
		dir = StringBytePtr(attr.Dir)
	}

	// Acquire the fork lock so that no other threads
	// create new fds that are not yet close-on-exec
	// before we fork.
	ForkLock.Lock()

	// Allocate child status pipe close on exec.
	if err = Pipe(p[0:]); err != 0 {
		goto error
	}
	if _, err = fcntl(p[0], F_SETFD, FD_CLOEXEC); err != 0 {
		goto error
	}
	if _, err = fcntl(p[1], F_SETFD, FD_CLOEXEC); err != 0 {
		goto error
	}

	// Kick off child.
	pid, err = forkAndExecInChild(argv0p, argvp, envvp, chroot, dir, attr, sys, p[1])
	if err != 0 {
		goto error
	}
	ForkLock.Unlock()

	// Read child error status from pipe.
	Close(p[1])
	n, err = raw_read(p[0], (*byte)(unsafe.Pointer(&err1)), int(unsafe.Sizeof(err1)))
	Close(p[0])
	if err != 0 || n != 0 {
		if n == int(unsafe.Sizeof(err1)) {
			err = int(err1)
		}
		if err == 0 {
			err = EPIPE
		}

		// Child failed; wait for it to exit, to make sure
		// the zombies don't accumulate.
		_, err1 := Wait4(pid, &wstatus, 0, nil)
		for err1 == EINTR {
			_, err1 = Wait4(pid, &wstatus, 0, nil)
		}
		return 0, err
	}

	// Read got EOF, so pipe closed on exec, so exec succeeded.
	return pid, 0

error:
	if p[0] >= 0 {
		Close(p[0])
		Close(p[1])
	}
	ForkLock.Unlock()
	return 0, err
}

// Combination of fork and exec, careful to be thread safe.
func ForkExec(argv0 string, argv []string, attr *ProcAttr) (pid int, err int) {
	return forkExec(argv0, argv, attr)
}

// StartProcess wraps ForkExec for package os.
func StartProcess(argv0 string, argv []string, attr *ProcAttr) (pid, handle int, err int) {
	pid, err = forkExec(argv0, argv, attr)
	return pid, 0, err
}

// Ordinary exec.
func Exec(argv0 string, argv []string, envv []string) (err int) {
	err1 := raw_execve(StringBytePtr(argv0),
		&StringSlicePtr(argv)[0],
		&StringSlicePtr(envv)[0])
	return int(err1)
}
