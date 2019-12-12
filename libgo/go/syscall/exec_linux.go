// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build linux

package syscall

import (
	"unsafe"
)

//sysnb	raw_prctl(option int, arg2 int, arg3 int, arg4 int, arg5 int) (ret int, err Errno)
//prctl(option _C_int, arg2 _C_long, arg3 _C_long, arg4 _C_long, arg5 _C_long) _C_int

//sysnb rawUnshare(flags int) (err Errno)
//unshare(flags _C_int) _C_int

//sysnb rawMount(source *byte, target *byte, fstype *byte, flags uintptr, data *byte) (err Errno)
//mount(source *byte, target *byte, fstype *byte, flags _C_long, data *byte) _C_int

//sysnb rawOpenat(dirfd int, pathname *byte, flags int, perm uint32) (fd int, err Errno)
//openat(dirfd _C_int, pathname *byte, flags _C_int, perm Mode_t) _C_int

// SysProcIDMap holds Container ID to Host ID mappings used for User Namespaces in Linux.
// See user_namespaces(7).
type SysProcIDMap struct {
	ContainerID int // Container ID.
	HostID      int // Host ID.
	Size        int // Size.
}

type SysProcAttr struct {
	Chroot     string      // Chroot.
	Credential *Credential // Credential.
	// Ptrace tells the child to call ptrace(PTRACE_TRACEME).
	// Call runtime.LockOSThread before starting a process with this set,
	// and don't call UnlockOSThread until done with PtraceSyscall calls.
	Ptrace       bool
	Setsid       bool           // Create session.
	Setpgid      bool           // Set process group ID to Pgid, or, if Pgid == 0, to new pid.
	Setctty      bool           // Set controlling terminal to fd Ctty (only meaningful if Setsid is set)
	Noctty       bool           // Detach fd 0 from controlling terminal
	Ctty         int            // Controlling TTY fd
	Foreground   bool           // Place child's process group in foreground. (Implies Setpgid. Uses Ctty as fd of controlling TTY)
	Pgid         int            // Child's process group ID if Setpgid.
	Pdeathsig    Signal         // Signal that the process will get when its parent dies (Linux only)
	Cloneflags   uintptr        // Flags for clone calls (Linux only)
	Unshareflags uintptr        // Flags for unshare calls (Linux only)
	UidMappings  []SysProcIDMap // User ID mappings for user namespaces.
	GidMappings  []SysProcIDMap // Group ID mappings for user namespaces.
	// GidMappingsEnableSetgroups enabling setgroups syscall.
	// If false, then setgroups syscall will be disabled for the child process.
	// This parameter is no-op if GidMappings == nil. Otherwise for unprivileged
	// users this should be set to false for mappings work.
	GidMappingsEnableSetgroups bool
	AmbientCaps                []uintptr // Ambient capabilities (Linux only)
}

var (
	none  = [...]byte{'n', 'o', 'n', 'e', 0}
	slash = [...]byte{'/', 0}
)

// Implemented in runtime package.
func runtime_BeforeFork()
func runtime_AfterFork()
func runtime_AfterForkInChild()

// Implemented in clone_linux.c
//go:noescape
func rawClone(flags _C_ulong, child_stack *byte, ptid *Pid_t, ctid *Pid_t, regs unsafe.Pointer) _C_long

// Fork, dup fd onto 0..len(fd), and exec(argv0, argvv, envv) in child.
// If a dup or exec fails, write the errno error to pipe.
// (Pipe is close-on-exec so if exec succeeds, it will be closed.)
// In the child, this function must not acquire any locks, because
// they might have been locked at the time of the fork. This means
// no rescheduling, no malloc calls, and no new stack segments.
// For the same reason compiler does not race instrument it.
// The calls to RawSyscall are okay because they are assembly
// functions that do not grow the stack.
//go:norace
func forkAndExecInChild(argv0 *byte, argv, envv []*byte, chroot, dir *byte, attr *ProcAttr, sys *SysProcAttr, pipe int) (pid int, err Errno) {
	// Set up and fork. This returns immediately in the parent or
	// if there's an error.
	r1, err1, p, locked := forkAndExecInChild1(argv0, argv, envv, chroot, dir, attr, sys, pipe)
	if locked {
		runtime_AfterFork()
	}
	if err1 != 0 {
		return 0, err1
	}

	// parent; return PID
	pid = int(r1)

	if sys.UidMappings != nil || sys.GidMappings != nil {
		Close(p[0])
		var err2 Errno
		// uid/gid mappings will be written after fork and unshare(2) for user
		// namespaces.
		if sys.Unshareflags&CLONE_NEWUSER == 0 {
			if err := writeUidGidMappings(pid, sys); err != nil {
				err2 = err.(Errno)
			}
		}
		raw_write(p[1], (*byte)(unsafe.Pointer(&err2)), int(unsafe.Sizeof(err2)))
		Close(p[1])
	}

	return pid, 0
}

const _LINUX_CAPABILITY_VERSION_3 = 0x20080522

type capHeader struct {
	version uint32
	pid     int32
}

type capData struct {
	effective   uint32
	permitted   uint32
	inheritable uint32
}
type caps struct {
	hdr  capHeader
	data [2]capData
}

// See CAP_TO_INDEX in linux/capability.h:
func capToIndex(cap uintptr) uintptr { return cap >> 5 }

// See CAP_TO_MASK in linux/capability.h:
func capToMask(cap uintptr) uint32 { return 1 << uint(cap&31) }

// forkAndExecInChild1 implements the body of forkAndExecInChild up to
// the parent's post-fork path. This is a separate function so we can
// separate the child's and parent's stack frames if we're using
// vfork.
//
// This is go:noinline because the point is to keep the stack frames
// of this and forkAndExecInChild separate.
//
//go:noinline
//go:norace
func forkAndExecInChild1(argv0 *byte, argv, envv []*byte, chroot, dir *byte, attr *ProcAttr, sys *SysProcAttr, pipe int) (r1 uintptr, err1 Errno, p [2]int, locked bool) {
	// Defined in linux/prctl.h starting with Linux 4.3.
	const (
		PR_CAP_AMBIENT       = 0x2f
		PR_CAP_AMBIENT_RAISE = 0x2
	)

	// vfork requires that the child not touch any of the parent's
	// active stack frames. Hence, the child does all post-fork
	// processing in this stack frame and never returns, while the
	// parent returns immediately from this frame and does all
	// post-fork processing in the outer frame.
	// Declare all variables at top in case any
	// declarations require heap allocation (e.g., err1).
	var (
		err2                      Errno
		nextfd                    int
		i                         int
		r2                        int
		caps                      caps
		fd1                       int
		puid, psetgroups, pgid    []byte
		uidmap, setgroups, gidmap []byte
	)

	if sys.UidMappings != nil {
		puid = []byte("/proc/self/uid_map\000")
		uidmap = formatIDMappings(sys.UidMappings)
	}

	if sys.GidMappings != nil {
		psetgroups = []byte("/proc/self/setgroups\000")
		pgid = []byte("/proc/self/gid_map\000")

		if sys.GidMappingsEnableSetgroups {
			setgroups = []byte("allow\000")
		} else {
			setgroups = []byte("deny\000")
		}
		gidmap = formatIDMappings(sys.GidMappings)
	}

	// Record parent PID so child can test if it has died.
	ppid := raw_getpid()

	// Guard against side effects of shuffling fds below.
	// Make sure that nextfd is beyond any currently open files so
	// that we can't run the risk of overwriting any of them.
	fd := make([]int, len(attr.Files))
	nextfd = len(attr.Files)
	for i, ufd := range attr.Files {
		if nextfd < int(ufd) {
			nextfd = int(ufd)
		}
		fd[i] = int(ufd)
	}
	nextfd++

	// Allocate another pipe for parent to child communication for
	// synchronizing writing of User ID/Group ID mappings.
	if sys.UidMappings != nil || sys.GidMappings != nil {
		if err := forkExecPipe(p[:]); err != nil {
			err1 = err.(Errno)
			return
		}
	}

	// About to call fork.
	// No more allocation or calls of non-assembly functions.
	runtime_BeforeFork()
	locked = true
	r2 = int(rawClone(_C_ulong(uintptr(SIGCHLD)|sys.Cloneflags), nil, nil, nil, unsafe.Pointer(nil)))
	if r2 < 0 {
		err1 = GetErrno()
	}
	if r2 != 0 {
		// If we're in the parent, we must return immediately
		// so we're not in the same stack frame as the child.
		// This can at most use the return PC, which the child
		// will not modify, and the results of
		// rawVforkSyscall, which must have been written after
		// the child was replaced.
		r1 = uintptr(r2)
		return
	}

	// Fork succeeded, now in child.

	runtime_AfterForkInChild()

	// Enable the "keep capabilities" flag to set ambient capabilities later.
	if len(sys.AmbientCaps) > 0 {
		_, err1 = raw_prctl(PR_SET_KEEPCAPS, 1, 0, 0, 0)
		if err1 != 0 {
			goto childerror
		}
	}

	// Wait for User ID/Group ID mappings to be written.
	if sys.UidMappings != nil || sys.GidMappings != nil {
		if err1 = raw_close(p[1]); err1 != 0 {
			goto childerror
		}
		r2, err1 = raw_read(p[0], (*byte)(unsafe.Pointer(&err2)), int(unsafe.Sizeof(err2)))
		if err1 != 0 {
			goto childerror
		}
		if r2 != int(unsafe.Sizeof(err2)) {
			err1 = EINVAL
			goto childerror
		}
		if err2 != 0 {
			err1 = err2
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
	if sys.Setpgid || sys.Foreground {
		// Place child in process group.
		err1 = raw_setpgid(0, sys.Pgid)
		if err1 != 0 {
			goto childerror
		}
	}

	if sys.Foreground {
		pgrp := Pid_t(sys.Pgid)
		if pgrp == 0 {
			pgrp = raw_getpid()
		}

		// Place process group in foreground.
		_, err1 = raw_ioctl_ptr(sys.Ctty, TIOCSPGRP, unsafe.Pointer(&pgrp))
		if err1 != 0 {
			goto childerror
		}
	}

	// Unshare
	if sys.Unshareflags != 0 {
		err1 = rawUnshare(int(sys.Unshareflags))
		if err1 != 0 {
			goto childerror
		}

		if sys.Unshareflags&CLONE_NEWUSER != 0 && sys.GidMappings != nil {
			dirfd := int(_AT_FDCWD)
			if fd1, err1 = rawOpenat(dirfd, &psetgroups[0], O_WRONLY, 0); err1 != 0 {
				goto childerror
			}
			_, err1 = raw_write(fd1, &setgroups[0], len(setgroups))
			if err1 != 0 {
				goto childerror
			}
			if err1 = raw_close(fd1); err1 != 0 {
				goto childerror
			}

			if fd1, err1 = rawOpenat(dirfd, &pgid[0], O_WRONLY, 0); err1 != 0 {
				goto childerror
			}
			_, err1 = raw_write(fd1, &gidmap[0], len(gidmap))
			if err1 != 0 {
				goto childerror
			}
			if err1 = raw_close(fd1); err1 != 0 {
				goto childerror
			}
		}

		if sys.Unshareflags&CLONE_NEWUSER != 0 && sys.UidMappings != nil {
			dirfd := int(_AT_FDCWD)
			if fd1, err1 = rawOpenat(dirfd, &puid[0], O_WRONLY, 0); err1 != 0 {
				goto childerror
			}
			_, err1 = raw_write(fd1, &uidmap[0], len(uidmap))
			if err1 != 0 {
				goto childerror
			}
			if err1 = raw_close(fd1); err1 != 0 {
				goto childerror
			}
		}

		// The unshare system call in Linux doesn't unshare mount points
		// mounted with --shared. Systemd mounts / with --shared. For a
		// long discussion of the pros and cons of this see debian bug 739593.
		// The Go model of unsharing is more like Plan 9, where you ask
		// to unshare and the namespaces are unconditionally unshared.
		// To make this model work we must further mark / as MS_PRIVATE.
		// This is what the standard unshare command does.
		if sys.Unshareflags&CLONE_NEWNS == CLONE_NEWNS {
			err1 = rawMount(&none[0], &slash[0], nil, MS_REC|MS_PRIVATE, nil)
			if err1 != 0 {
				goto childerror
			}
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
		var groups unsafe.Pointer
		if ngroups > 0 {
			groups = unsafe.Pointer(&cred.Groups[0])
		}
		if !(sys.GidMappings != nil && !sys.GidMappingsEnableSetgroups && ngroups == 0) && !cred.NoSetGroups {
			err1 = raw_setgroups(ngroups, groups)
			if err1 != 0 {
				goto childerror
			}
		}
		_, _, err1 = RawSyscall(sys_SETGID, uintptr(cred.Gid), 0, 0)
		if err1 != 0 {
			goto childerror
		}
		_, _, err1 = RawSyscall(sys_SETUID, uintptr(cred.Uid), 0, 0)
		if err1 != 0 {
			goto childerror
		}
	}

	if len(sys.AmbientCaps) != 0 {
		// Ambient capabilities were added in the 4.3 kernel,
		// so it is safe to always use _LINUX_CAPABILITY_VERSION_3.
		caps.hdr.version = _LINUX_CAPABILITY_VERSION_3

		if _, _, err1 = RawSyscall(SYS_CAPGET, uintptr(unsafe.Pointer(&caps.hdr)), uintptr(unsafe.Pointer(&caps.data[0])), 0); err1 != 0 {
			goto childerror
		}

		for _, c := range sys.AmbientCaps {
			// Add the c capability to the permitted and inheritable capability mask,
			// otherwise we will not be able to add it to the ambient capability mask.
			caps.data[capToIndex(c)].permitted |= capToMask(c)
			caps.data[capToIndex(c)].inheritable |= capToMask(c)
		}

		if _, _, err1 = RawSyscall(SYS_CAPSET, uintptr(unsafe.Pointer(&caps.hdr)), uintptr(unsafe.Pointer(&caps.data[0])), 0); err1 != 0 {
			goto childerror
		}

		for _, c := range sys.AmbientCaps {
			_, _, err1 = RawSyscall6(SYS_PRCTL, PR_CAP_AMBIENT, uintptr(PR_CAP_AMBIENT_RAISE), c, 0, 0, 0)
			if err1 != 0 {
				goto childerror
			}
		}
	}

	// Chdir
	if dir != nil {
		err1 = raw_chdir(dir)
		if err1 != 0 {
			goto childerror
		}
	}

	// Parent death signal
	if sys.Pdeathsig != 0 {
		_, err1 = raw_prctl(PR_SET_PDEATHSIG, int(sys.Pdeathsig), 0, 0, 0)
		if err1 != 0 {
			goto childerror
		}

		// Signal self if parent is already dead. This might cause a
		// duplicate signal in rare cases, but it won't matter when
		// using SIGKILL.
		r1 := raw_getppid()
		if r1 != ppid {
			pid := raw_getpid()
			err1 = raw_kill(pid, sys.Pdeathsig)
			if err1 != 0 {
				goto childerror
			}
		}
	}

	// Pass 1: look for fd[i] < i and move those up above len(fd)
	// so that pass 2 won't stomp on an fd it needs later.
	if pipe < nextfd {
		err1 = raw_dup2(pipe, nextfd)
		if err1 != 0 {
			goto childerror
		}
		raw_fcntl(nextfd, F_SETFD, FD_CLOEXEC)
		pipe = nextfd
		nextfd++
	}
	for i = 0; i < len(fd); i++ {
		if fd[i] >= 0 && fd[i] < int(i) {
			if nextfd == pipe { // don't stomp on pipe
				nextfd++
			}
			err1 = raw_dup2(fd[i], nextfd)
			if err1 != 0 {
				goto childerror
			}
			raw_fcntl(nextfd, F_SETFD, FD_CLOEXEC)
			fd[i] = nextfd
			nextfd++
		}
	}

	// Pass 2: dup fd[i] down onto i.
	for i = 0; i < len(fd); i++ {
		if fd[i] == -1 {
			raw_close(i)
			continue
		}
		if fd[i] == int(i) {
			// dup2(i, i) won't clear close-on-exec flag on Linux,
			// probably not elsewhere either.
			_, err1 = raw_fcntl(fd[i], F_SETFD, 0)
			if err1 != 0 {
				goto childerror
			}
			continue
		}
		// The new fd is created NOT close-on-exec,
		// which is exactly what we want.
		err1 = raw_dup2(fd[i], i)
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

	// Set the controlling TTY to Ctty
	if sys.Setctty {
		_, err1 = raw_ioctl(sys.Ctty, TIOCSCTTY, 1)
		if err1 != 0 {
			goto childerror
		}
	}

	// Enable tracing if requested.
	// Do this right before exec so that we don't unnecessarily trace the runtime
	// setting up after the fork. See issue #21428.
	if sys.Ptrace {
		err1 = raw_ptrace(_PTRACE_TRACEME, 0, nil, nil)
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
}

// Try to open a pipe with O_CLOEXEC set on both file descriptors.
func forkExecPipe(p []int) (err error) {
	err = Pipe2(p, O_CLOEXEC)
	// pipe2 was added in 2.6.27 and our minimum requirement is 2.6.23, so it
	// might not be implemented.
	if err == ENOSYS {
		if err = Pipe(p); err != nil {
			return
		}
		if _, err = fcntl(p[0], F_SETFD, FD_CLOEXEC); err != nil {
			return
		}
		_, err = fcntl(p[1], F_SETFD, FD_CLOEXEC)
	}
	return
}

func formatIDMappings(idMap []SysProcIDMap) []byte {
	var data []byte
	for _, im := range idMap {
		data = append(data, []byte(itoa(im.ContainerID)+" "+itoa(im.HostID)+" "+itoa(im.Size)+"\n")...)
	}
	return data
}

// writeIDMappings writes the user namespace User ID or Group ID mappings to the specified path.
func writeIDMappings(path string, idMap []SysProcIDMap) error {
	fd, err := Open(path, O_RDWR, 0)
	if err != nil {
		return err
	}

	if _, err := Write(fd, formatIDMappings(idMap)); err != nil {
		Close(fd)
		return err
	}

	if err := Close(fd); err != nil {
		return err
	}

	return nil
}

// writeSetgroups writes to /proc/PID/setgroups "deny" if enable is false
// and "allow" if enable is true.
// This is needed since kernel 3.19, because you can't write gid_map without
// disabling setgroups() system call.
func writeSetgroups(pid int, enable bool) error {
	sgf := "/proc/" + itoa(pid) + "/setgroups"
	fd, err := Open(sgf, O_RDWR, 0)
	if err != nil {
		return err
	}

	var data []byte
	if enable {
		data = []byte("allow")
	} else {
		data = []byte("deny")
	}

	if _, err := Write(fd, data); err != nil {
		Close(fd)
		return err
	}

	return Close(fd)
}

// writeUidGidMappings writes User ID and Group ID mappings for user namespaces
// for a process and it is called from the parent process.
func writeUidGidMappings(pid int, sys *SysProcAttr) error {
	if sys.UidMappings != nil {
		uidf := "/proc/" + itoa(pid) + "/uid_map"
		if err := writeIDMappings(uidf, sys.UidMappings); err != nil {
			return err
		}
	}

	if sys.GidMappings != nil {
		// If the kernel is too old to support /proc/PID/setgroups, writeSetGroups will return ENOENT; this is OK.
		if err := writeSetgroups(pid, sys.GidMappingsEnableSetgroups); err != nil && err != ENOENT {
			return err
		}
		gidf := "/proc/" + itoa(pid) + "/gid_map"
		if err := writeIDMappings(gidf, sys.GidMappings); err != nil {
			return err
		}
	}

	return nil
}
