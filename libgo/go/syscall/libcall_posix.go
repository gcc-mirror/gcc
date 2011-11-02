// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// POSIX library calls.
// This file is compiled as ordinary Go code,
// but it is also input to mksyscall,
// which parses the //sys lines and generates library call stubs.
// Note that sometimes we use a lowercase //sys name and
// wrap it in our own nicer implementation.

package syscall

import "unsafe"

/*
 * Wrapped
 */

//sysnb	pipe(p *[2]int) (errno int)
//pipe(p *[2]int) int
func Pipe(p []int) (errno int) {
	if len(p) != 2 {
		return EINVAL
	}
	var pp [2]int
	errno = pipe(&pp)
	p[0] = pp[0]
	p[1] = pp[1]
	return
}

//sys	utimes(path string, times *[2]Timeval) (errno int)
//utimes(path *byte, times *[2]Timeval) int
func Utimes(path string, tv []Timeval) (errno int) {
	if len(tv) != 2 {
		return EINVAL
	}
	return utimes(path, (*[2]Timeval)(unsafe.Pointer(&tv[0])))
}

//sys	getcwd(buf *byte, size Size_t) (errno int)
//getcwd(buf *byte, size Size_t) *byte

const ImplementsGetwd = true

func Getwd() (ret string, errno int) {
	for len := Size_t(4096); ; len *= 2 {
		b := make([]byte, len)
		err := getcwd(&b[0], len)
		if err == 0 {
			i := 0;
			for b[i] != 0 {
				i++;
			}
			return string(b[0:i]), 0;
		}
		if err != ERANGE {
			return "", err
		}
	}
}

//sysnb	getgroups(size int, list *Gid_t) (nn int, errno int)
//getgroups(size int, list *Gid_t) int

func Getgroups() (gids []int, errno int) {
	n, err := getgroups(0, nil)
	if err != 0 {
		return nil, errno
	}
	if n == 0 {
		return nil, 0
	}

	// Sanity check group count.  Max is 1<<16 on GNU/Linux.
	if n < 0 || n > 1<<20 {
		return nil, EINVAL
	}

	a := make([]Gid_t, n)
	n, err = getgroups(n, &a[0])
	if err != 0 {
		return nil, errno
	}
	gids = make([]int, n)
	for i, v := range a[0:n] {
		gids[i] = int(v)
	}
	return
}

//sysnb	setgroups(n int, list *Gid_t) (errno int)
//setgroups(n Size_t, list *Gid_t) int

func Setgroups(gids []int) (errno int) {
	if len(gids) == 0 {
		return setgroups(0, nil)
	}

	a := make([]Gid_t, len(gids))
	for i, v := range gids {
		a[i] = Gid_t(v)
	}
	return setgroups(len(a), &a[0])
}

type WaitStatus uint32

// The WaitStatus methods are implemented in C, to pick up the macros
// #defines in <sys/wait.h>.

func (w WaitStatus) Exited() bool
func (w WaitStatus) Signaled() bool
func (w WaitStatus) Stopped() bool
func (w WaitStatus) Continued() bool
func (w WaitStatus) CoreDump() bool
func (w WaitStatus) ExitStatus() int
func (w WaitStatus) Signal() int
func (w WaitStatus) StopSignal() int
func (w WaitStatus) TrapCause() int

//sys	Mkfifo(path string, mode uint32) (errno int)
//mkfifo(path *byte, mode Mode_t) int

//sys	Select(nfd int, r *FdSet, w *FdSet, e *FdSet, timeout *Timeval) (n int, errno int)
//select(nfd int, r *FdSet, w *FdSet, e *FdSet, timeout *Timeval) int

const nfdbits = unsafe.Sizeof(fds_bits_type) * 8

type FdSet struct {
	Bits [(FD_SETSIZE + nfdbits - 1) / nfdbits]fds_bits_type
}

func FDSet(fd int, set *FdSet) {
	set.Bits[fd / nfdbits] |= (1 << (uint)(fd % nfdbits))
}

func FDClr(fd int, set *FdSet) {
	set.Bits[fd / nfdbits] &^= (1 << (uint)(fd % nfdbits))
}

func FDIsSet(fd int, set *FdSet) bool {
	if set.Bits[fd / nfdbits] & (1 << (uint)(fd % nfdbits)) != 0 {
		return true
	} else {
		return false
	}
}

func FDZero(set *FdSet) {
	for i := range set.Bits {
		set.Bits[i] = 0
	}
}

//sys	Access(path string, mode uint32) (errno int)
//access(path *byte, mode int) int

//sys	Chdir(path string) (errno int)
//chdir(path *byte) int

//sys	Chmod(path string, mode uint32) (errno int)
//chmod(path *byte, mode Mode_t) int

//sys	Chown(path string, uid int, gid int) (errno int)
//chown(path *byte, uid Uid_t, gid Gid_t) int

//sys	Chroot(path string) (errno int)
//chroot(path *byte) int

//sys	Close(fd int) (errno int)
//close(fd int) int

//sys	Creat(path string, mode uint32) (fd int, errno int)
//creat(path *byte, mode Mode_t) int

//sysnb	Dup(oldfd int) (fd int, errno int)
//dup(oldfd int) int

//sysnb	Dup2(oldfd int, newfd int) (fd int, errno int)
//dup2(oldfd int, newfd int) int

//sys	Exit(code int)
//exit(code int)

//sys	Fchdir(fd int) (errno int)
//fchdir(fd int) int

//sys	Fchmod(fd int, mode uint32) (errno int)
//fchmod(fd int, mode Mode_t) int

//sys	Fchown(fd int, uid int, gid int) (errno int)
//fchown(fd int, uid Uid_t, gid Gid_t) int

//sys	fcntl(fd int, cmd int, arg int) (val int, errno int)
//fcntl(fd int, cmd int, arg int) int

//sys	Fdatasync(fd int) (errno int)
//fdatasync(fd int) int

//sys	Fsync(fd int) (errno int)
//fsync(fd int) int

//sysnb Getegid() (egid int)
//getegid() Gid_t

//sysnb Geteuid() (euid int)
//geteuid() Uid_t

//sysnb Getgid() (gid int)
//getgid() Gid_t

//sysnb	Getpagesize() (pagesize int)
//getpagesize() int

//sysnb	Getpgid(pid int) (pgid int, errno int)
//getpgid(pid Pid_t) Pid_t

//sysnb	Getpgrp() (pid int)
//getpgrp() Pid_t

//sysnb	Getpid() (pid int)
//getpid() Pid_t

//sysnb	Getppid() (ppid int)
//getppid() Pid_t

// FIXME: mksysinfo Rlimit
// //sysnb	Getrlimit(resource int, rlim *Rlimit) (errno int)
// //getrlimit(resource int, rlim *Rlimit) int

//sysnb	Getrusage(who int, rusage *Rusage) (errno int)
//getrusage(who int, rusage *Rusage) int

//sysnb	gettimeofday(tv *Timeval, tz *byte) (errno int)
//gettimeofday(tv *Timeval, tz *byte) int
func Gettimeofday(tv *Timeval) (errno int) {
	return gettimeofday(tv, nil)
}

//sysnb Getuid() (uid int)
//getuid() Uid_t

//sysnb	Kill(pid int, sig int) (errno int)
//kill(pid Pid_t, sig int) int

//sys	Lchown(path string, uid int, gid int) (errno int)
//lchown(path *byte, uid Uid_t, gid Gid_t) int

//sys	Link(oldpath string, newpath string) (errno int)
//link(oldpath *byte, newpath *byte) int

//sys	Mkdir(path string, mode uint32) (errno int)
//mkdir(path *byte, mode Mode_t) int

//sys	Mknod(path string, mode uint32, dev int) (errno int)
//mknod(path *byte, mode Mode_t, dev _dev_t) int

//sys	Mount(source string, target string, fstype string, flags int, data string) (errno int)
//mount(source *byte, target *byte, fstype *byte, flags _C_long, data *byte) int

//sys	Nanosleep(time *Timespec, leftover *Timespec) (errno int)
//nanosleep(time *Timespec, leftover *Timespec) int

//sys	Pause() (errno int)
//pause() int

//sys	Read(fd int, p []byte) (n int, errno int)
//read(fd int, buf *byte, count Size_t) Ssize_t

//sys	Readlink(path string, buf []byte) (n int, errno int)
//readlink(path *byte, buf *byte, bufsiz Size_t) Ssize_t

//sys	Rename(oldpath string, newpath string) (errno int)
//rename(oldpath *byte, newpath *byte) int

//sys	Rmdir(path string) (errno int)
//rmdir(path *byte) int

//sys	Setdomainname(p []byte) (errno int)
//setdomainname(name *byte, len Size_t) int

//sys	Sethostname(p []byte) (errno int)
//sethostname(name *byte, len Size_t) int

//sysnb	Setgid(gid int) (errno int)
//setgid(gid Gid_t) int

//sysnb Setregid(rgid int, egid int) (errno int)
//setregid(rgid Gid_t, egid Gid_t) int

//sysnb	Setpgid(pid int, pgid int) (errno int)
//setpgid(pid Pid_t, pgid Pid_t) int

//sysnb	Setreuid(ruid int, euid int) (errno int)
//setreuid(ruid Uid_t, euid Uid_t) int

// FIXME: mksysinfo Rlimit
// //sysnb	Setrlimit(resource int, rlim *Rlimit) (errno int)
// //setrlimit(resource int, rlim *Rlimit) int

//sysnb	Setsid() (pid int, errno int)
//setsid() Pid_t

//sysnb	settimeofday(tv *Timeval, tz *byte) (errno int)
//settimeofday(tv *Timeval, tz *byte) int

func Settimeofday(tv *Timeval) (errno int) {
	return settimeofday(tv, nil)
}

//sysnb	Setuid(uid int) (errno int)
//setuid(uid Uid_t) int

//sys	Symlink(oldpath string, newpath string) (errno int)
//symlink(oldpath *byte, newpath *byte) int

//sys	Sync()
//sync()

// FIXME: mksysinfo Time_t
// //sysnb	Time(t *Time_t) (tt Time_t, errno int)
// //time(t *Time_t) Time_t

// FIXME: mksysinfo Tms
// //sysnb	Times(tms *Tms) (ticks uintptr, errno int)
// //times(tms *Tms) _clock_t

//sysnb	Umask(mask int) (oldmask int)
//umask(mask Mode_t) Mode_t

//sys	Unlink(path string) (errno int)
//unlink(path *byte) int

// FIXME: mksysinfo Utimbuf
// //sys	Utime(path string, buf *Utimbuf) (errno int)
// //utime(path *byte, buf *Utimbuf) int

//sys	Write(fd int, p []byte) (n int, errno int)
//write(fd int, buf *byte, count Size_t) Ssize_t

//sys	munmap(addr uintptr, length uintptr) (errno int)
//munmap(addr *byte, length Size_t) int

//sys Madvise(b []byte, advice int) (errno int)
//madvise(addr *byte, len Size_t, advice int) int

//sys	Mprotect(b []byte, prot int) (errno int)
//mprotect(addr *byte, len Size_t, prot int) int

//sys	Mlock(b []byte) (errno int)
//mlock(addr *byte, len Size_t) int

//sys	Munlock(b []byte) (errno int)
//munlock(addr *byte, len Size_t) int

//sys	Mlockall(flags int) (errno int)
//mlockall(flags int) int

//sys	Munlockall() (errno int)
//munlockall() int

func TimespecToNsec(ts Timespec) int64 { return int64(ts.Sec)*1e9 + int64(ts.Nsec) }

func NsecToTimespec(nsec int64) (ts Timespec) {
	ts.Sec = Timespec_sec_t(nsec / 1e9)
	ts.Nsec = Timespec_nsec_t(nsec % 1e9)
	return
}

func TimevalToNsec(tv Timeval) int64 { return int64(tv.Sec)*1e9 + int64(tv.Usec)*1e3 }

func NsecToTimeval(nsec int64) (tv Timeval) {
	nsec += 999 // round up to microsecond
	tv.Sec = Timeval_sec_t(nsec / 1e9)
	tv.Usec = Timeval_usec_t(nsec % 1e9 / 1e3)
	return
}

//sysnb	Tcgetattr(fd int, p *Termios) (errno int)
//tcgetattr(fd int, p *Termios) int

//sys	Tcsetattr(fd int, actions int, p *Termios) (errno int)
//tcsetattr(fd int, actions int, p *Termios) int
