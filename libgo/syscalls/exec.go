// exec.go -- fork/exec syscall support.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Fork, exec, wait, etc.

package syscall

import "unsafe"

func libc_fcntl(fd int, cmd int, arg int) int __asm__ ("fcntl")
func libc_fork() Pid_t __asm__ ("fork")
func libc_setsid() Pid_t __asm__ ("setsid")
func libc_setpgid(Pid_t, Pid_t) int __asm__ ("setpgid")
func libc_chroot(path *byte) int __asm__ ("chroot")
func libc_setuid(Uid_t) int __asm__ ("setuid")
func libc_setgid(Gid_t) int __asm__ ("setgid")
func libc_setgroups(Size_t, *Gid_t) int __asm__ ("setgroups")
func libc_chdir(name *byte) int __asm__ ("chdir")
func libc_dup2(int, int) int __asm__ ("dup2")
func libc_ioctl(int, int) int __asm__ ("ioctl")
func libc_execve(*byte, **byte, **byte) int __asm__ ("execve")
func libc_sysexit(int) __asm__ ("_exit")

// Fork, dup fd onto 0..len(fd), and exec(argv0, argvv, envv) in child.
// If a dup or exec fails, write the errno int to pipe.
// (Pipe is close-on-exec so if exec succeeds, it will be closed.)
// In the child, this function must not acquire any locks, because
// they might have been locked at the time of the fork.  This means
// no rescheduling, no malloc calls, and no new stack segments.
func forkAndExecInChild(argv0 *byte, argv, envv []*byte, chroot, dir *byte, attr *ProcAttr, sys *SysProcAttr, pipe int) (pid int, err int) {
	// Declare all variables at top in case any
	// declarations require heap allocation (e.g., err1).
	var r1, r2, err1 uintptr
	var nextfd int
	var i int

	// guard against side effects of shuffling fds below.
	fd := append([]int(nil), attr.Files...)

	darwin := OS == "darwin"

	// About to call fork.
	// No more allocation or calls of non-assembly functions.
	child := libc_fork()
	if child == -1 {
		return 0, GetErrno()
	}

	if child != 0 {
		// parent; return PID
		return int(child), 0
	}

	// Fork succeeded, now in child.

	// Enable tracing if requested.
	if sys.Ptrace {
		if libc_ptrace(_PTRACE_TRACEME, 0, 0, nil) < 0 {
			goto childerror
		}
	}

	// Session ID
	if sys.Setsid {
		if libc_setsid() == Pid_t(-1) {
			goto childerror
		}
	}

	// Set process group
	if sys.Setpgid {
		if libc_setpgid(0, 0) < 0 {
			goto childerror
		}
	}

	// Chroot
	if chroot != nil {
		if libc_chroot(chroot) < 0 {
			goto childerror
		}
	}

	// User and groups
	if cred := sys.Credential; cred != nil {
		ngroups := uintptr(len(cred.Groups))
		var groups *Gid_t
		if ngroups > 0 {
			groups = (*Gid_t)(unsafe.Pointer(&cred.Groups[0]))
		}
		if libc_setgroups(Size_t(ngroups), groups) < 0 {
			goto childerror
		}
		if libc_setgid(Gid_t(cred.Gid)) < 0 {
			goto childerror
		}
		if libc_setuid(Uid_t(cred.Uid)) < 0 {
			goto childerror
		}
	}

	// Chdir
	if dir != nil {
		if libc_chdir(dir) < 0 {
			goto childerror
		}
	}

	// Pass 1: look for fd[i] < i and move those up above len(fd)
	// so that pass 2 won't stomp on an fd it needs later.
	nextfd = int(len(fd))
	if pipe < nextfd {
		r := libc_dup2(pipe, nextfd)
		if r == -1 {
			goto childerror
		}
		libc_fcntl(nextfd, F_SETFD, FD_CLOEXEC)
		pipe = nextfd
		nextfd++
	}
	for i = 0; i < len(fd); i++ {
		if fd[i] >= 0 && fd[i] < int(i) {
			r := libc_dup2(fd[i], nextfd)
			if r == -1 {
				goto childerror
			}
			libc_fcntl(nextfd, F_SETFD, FD_CLOEXEC)
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
			libc_close(i)
			continue
		}
		if fd[i] == int(i) {
			// dup2(i, i) won't clear close-on-exec flag on Linux,
			// probably not elsewhere either.
			r := libc_fcntl(fd[i], F_SETFD, 0)
			if r != 0 {
				goto childerror
			}
			continue
		}
		// The new fd is created NOT close-on-exec,
		// which is exactly what we want.
		r := libc_dup2(fd[i], i)
		if r == -1 {
			goto childerror
		}
	}

	// By convention, we don't close-on-exec the fds we are
	// started with, so if len(fd) < 3, close 0, 1, 2 as needed.
	// Programs that know they inherit fds >= 3 will need
	// to set them close-on-exec.
	for i = len(fd); i < 3; i++ {
		libc_close(i)
	}

	// Detach fd 0 from tty
	if sys.Noctty {
		if libc_ioctl(0, TIOCNOTTY) < 0 {
			goto childerror
		}
	}

	// Make fd 0 the tty
	if sys.Setctty {
		if libc_ioctl(0, TIOCSCTTY) < 0 {
			goto childerror
		}
	}

	// Time to exec.
	libc_execve(argv0, &argv[0], &envv[0])

childerror:
	// send error code on pipe
	var e uintptr = uintptr(GetErrno())
	libc_write(pipe, (*byte)(unsafe.Pointer(&e)),
		   Size_t(unsafe.Sizeof(err1)))
	for {
		libc_sysexit(253)
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
	var n Ssize_t
	var r1 int
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
	argvp := StringArrayPtr(argv)
	envvp := StringArrayPtr(attr.Env)

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
	n = libc_read(p[0], (*byte)(unsafe.Pointer(&err1)),
		      Size_t(unsafe.Sizeof(err1)))
	err = 0
	if n < 0 {
		err = GetErrno()
	}
	Close(p[0])
	if err != 0 || n != 0 {
		if int(n) == unsafe.Sizeof(err1) {
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
	argv_arg := StringArrayPtr(argv)
	envv_arg := StringArrayPtr(envv)
	libc_execve(StringBytePtr(argv0), &argv_arg[0], &envv_arg[0])
	return GetErrno()
}
