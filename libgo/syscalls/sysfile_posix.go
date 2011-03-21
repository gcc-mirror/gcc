// sysfile_posix.go -- POSIX File handling.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Support for basic Unix file operations.  This file simply
// translates from Go data types to Unix data types, and handles
// errno.  FIXME: This could probably be done mechanically.

package syscall

import "unsafe"

func libc_open(name *byte, mode int, perm Mode_t) int __asm__ ("open");
func libc_close(fd int) int __asm__ ("close");
func libc_read(fd int, buf *byte, count Size_t) Ssize_t __asm__ ("read");
func libc_write(fd int, buf *byte, count Size_t) Ssize_t __asm__ ("write");
func libc_fsync(fd int) int __asm__ ("fsync")
func libc_pipe(filedes *int) int __asm__("pipe");
func libc_unlink(name *byte) int __asm__ ("unlink");
func libc_rmdir(name *byte) int __asm__ ("rmdir");
func libc_fcntl(fd int, cmd int, arg int) int __asm__ ("fcntl");
func libc_mkdir(name *byte, perm Mode_t) int __asm__ ("mkdir");
func libc_dup(int) int __asm__ ("dup")
func libc_gettimeofday(tv *Timeval, tz *byte) int __asm__ ("gettimeofday");
func libc_select(int, *byte, *byte, *byte, *Timeval) int __asm__ ("select");
func libc_chdir(name *byte) int __asm__ ("chdir");
func libc_fchdir(int) int __asm__ ("fchdir");
func libc_getcwd(*byte, Size_t) *byte __asm__ ("getcwd");
func libc_link(oldpath *byte, newpath *byte) int __asm__ ("link");
func libc_symlink(oldpath *byte, newpath *byte) int __asm__ ("symlink");
func libc_readlink(*byte, *byte, Size_t) Ssize_t  __asm__ ("readlink");
func libc_rename(oldpath *byte, newpath *byte) int __asm__ ("rename");
func libc_chmod(path *byte, mode Mode_t) int __asm__ ("chmod");
func libc_fchmod(fd int, mode Mode_t) int __asm__ ("fchmod");
func libc_chown(path *byte, owner Uid_t, group Gid_t) int __asm__ ("chown");
func libc_fchown(fd int, owner Uid_t, group Gid_t) int __asm__ ("fchown");
func libc_lchown(path *byte, owner Uid_t, group Gid_t) int __asm__ ("lchown");
func libc_utimes(filename *byte, times *[2]Timeval) int __asm__ ("utimes");
func libc_getuid() Uid_t __asm__ ("getuid");
func libc_geteuid() Uid_t __asm__ ("geteuid");
func libc_getgid() Gid_t __asm__ ("getgid");
func libc_getegid() Gid_t __asm__ ("getegid");
func libc_getgroups(size int, list *Gid_t) int __asm__ ("getgroups");
func libc_getpagesize() int __asm__ ("getpagesize");
func libc_exit(status int) __asm__ ("exit")
func libc_getpid() Pid_t __asm__ ("getpid")
func libc_getppid() Pid_t __asm__ ("getppid")
func libc_kill(Pid_t, int) int __asm__ ("kill")

func Open(name string, mode int, perm uint32) (fd int, errno int) {
  fd = libc_open(StringBytePtr(name), mode, Mode_t(perm));
  if fd < 0 { errno = GetErrno() }
  return;
}

func Creat(name string, perm uint32) (fd int, errno int) {
  fd = libc_open(StringBytePtr(name), O_CREAT | O_WRONLY | O_TRUNC, Mode_t(perm));
  if fd < 0 { errno = GetErrno() }
  return;
}

func Close(fd int) (errno int) {
  r := libc_close(fd);
  if r < 0 { errno = GetErrno() }
  return;
}

func Read(fd int, p []byte) (n int, errno int) {
  var _p0 *byte;
  if len(p) > 0 { _p0 = &p[0]; }
  r := libc_read(fd, _p0, Size_t(len(p)));
  if r == -1 { errno = GetErrno() }
  n = int(r);
  return;
}

func Write(fd int, p []byte) (n int, errno int) {
  var _p0 *byte;
  if len(p) > 0 { _p0 = &p[0]; }
  r := libc_write(fd, _p0, Size_t(len(p)));
  if r == -1 { errno = GetErrno() }
  n = int(r);
  return;
}

func Fsync(fd int) (errno int) {
	if libc_fsync(fd) < 0 {
		errno = GetErrno()
	}
	return
}

func Pread(fd int, p []byte, offset int64) (n int, errno int) {
  var _p0 *byte;
  if len(p) > 0 { _p0 = &p[0]; }
  r := libc_pread(fd, _p0, Size_t(len(p)), Offset_t(offset));
  if r == -1 { errno = GetErrno() }
  n = int(r);
  return;
}

func Pwrite(fd int, p []byte, offset int64) (n int, errno int) {
  var _p0 *byte;
  if len(p) > 0 { _p0 = &p[0]; }
  r := libc_pwrite(fd, _p0, Size_t(len(p)), Offset_t(offset));
  if r == -1 { errno = GetErrno() }
  n = int(r);
  return;
}

func Seek(fd int, offset int64, whence int) (off int64, errno int) {
  r := libc_lseek(fd, Offset_t(offset), whence);
  if r == -1 { errno = GetErrno() }
  off = int64(r);
  return;
}

func Pipe(p []int) (errno int) {
  if len(p) != 2 {
    return EINVAL;
  }
  var pp [2]int;
  r := libc_pipe(&pp[0]);
  if r < 0 {
    errno = GetErrno();
  }
  p[0] = pp[0];
  p[1] = pp[1];
  return;
}

func Stat(name string, buf *Stat_t) (errno int) {
  r := libc_stat(StringBytePtr(name), buf);
  if r < 0 { errno = GetErrno() }
  return;
}

func Lstat(name string, buf *Stat_t) (errno int) {
  r := libc_lstat(StringBytePtr(name), buf);
  if r < 0 { errno = GetErrno() }
  return;
}

func Fstat(fd int, buf *Stat_t) (errno int) {
  r := libc_fstat(fd, buf);
  if r < 0 { errno = GetErrno() }
  return;
}

func Unlink(name string) (errno int) {
  r := libc_unlink(StringBytePtr(name));
  if r < 0 { errno = GetErrno() }
  return;
}

func Rmdir(name string) (errno int) {
  r := libc_rmdir(StringBytePtr(name));
  if r < 0 { errno = GetErrno() }
  return;
}

func Mkdir(path string, mode uint32) (errno int) {
  r := libc_mkdir(StringBytePtr(path), Mode_t(mode));
  if r < 0 { errno = GetErrno() }
  return;
}

func Dup(oldfd int) (fd int, errno int) {
	fd = libc_dup(oldfd)
	if fd < 0 {
		errno = GetErrno()
	}
	return
}

func Gettimeofday(tv *Timeval) (errno int) {
  r := libc_gettimeofday(tv, nil);
  if r < 0 { errno = GetErrno() }
  return;
}

type FdSet_t struct {
	Fds_bits [(FD_SETSIZE + 63) / 64]int64;
}

func FDSet(fd int, set *FdSet_t) {
	set.Fds_bits[fd / 64] |= (1 << (uint)(fd % 64))
}

func FDClr(fd int, set *FdSet_t) {
	set.Fds_bits[fd / 64] &= ^(1 << (uint)(fd % 64))
}

func FDIsSet(fd int, set *FdSet_t) bool {
	if set.Fds_bits[fd / 64] & (1 << (uint)(fd % 64)) != 0 {
		return true
	} else {
		return false
	}
}

func FDZero(set *FdSet_t) {
	for i := 0; i < ((FD_SETSIZE + 63) / 64); i++ {
		set.Fds_bits[i] = 0
	}
}

func Select(nfds int, r *FdSet_t, w *FdSet_t, e *FdSet_t, timeout *Timeval) (n int, errno int) {
  n = libc_select(nfds, (*byte)(unsafe.Pointer(r)),
		  (*byte)(unsafe.Pointer(e)),
		  (*byte)(unsafe.Pointer(e)), timeout);
  if n < 0 { errno = GetErrno() }
  return;
}

func Chdir(path string) (errno int) {
  r := libc_chdir(StringBytePtr(path));
  if r < 0 { errno = GetErrno() }
  return;
}

func Fchdir(fd int) (errno int) {
  r := libc_fchdir(int(fd));
  if r < 0 { errno = GetErrno() }
  return;
}

const ImplementsGetwd = true

func Getwd() (ret string, errno int) {
	for len := Size_t(4096); ; len *= 2 {
		b := make([]byte, len);
		p := libc_getcwd(&b[0], len);
		if p != nil {
			i := 0;
			for b[i] != 0 {
				i++;
			}
			return string(b[0:i]), 0;
		}
		e := GetErrno();
		if e != ERANGE {
			return "", e;
		}
	}
}

func Link(oldpath, newpath string) (errno int) {
  r := libc_link(StringBytePtr(oldpath), StringBytePtr(newpath));
  if r < 0 { errno = GetErrno() }
  return;
}

func Symlink(oldpath, newpath string) (errno int) {
  r := libc_symlink(StringBytePtr(oldpath), StringBytePtr(newpath));
  if r < 0 { errno = GetErrno() }
  return;
}

func Readlink(path string, buf []byte) (n int, errno int) {
  var _p0 *byte;
  if len(buf) > 0 { _p0 = &buf[0]; }
  r := libc_readlink(StringBytePtr(path), _p0, Size_t(len(buf)));
  if r < 0 { errno = GetErrno() }
  n = int(r);
  return;
}

func Rename(oldpath, newpath string) (errno int) {
	r := libc_rename(StringBytePtr(oldpath), StringBytePtr(newpath));
	if r < 0 { errno = GetErrno() }
	return 
}

func Chmod(path string, mode uint32) (errno int) {
  r := libc_chmod(StringBytePtr(path), Mode_t(mode));
  if r < 0 { errno = GetErrno() }
  return;
}

func Fchmod(fd int, mode uint32) (errno int) {
  r := libc_fchmod(fd, Mode_t(mode));
  if r < 0 { errno = GetErrno() }
  return;
}

func Chown(path string, uid int, gid int) (errno int) {
  r := libc_chown(StringBytePtr(path), Uid_t(uid), Gid_t(gid));
  if r < 0 { errno = GetErrno() }
  return;
}

func Lchown(path string, uid int, gid int) (errno int) {
  r := libc_lchown(StringBytePtr(path), Uid_t(uid), Gid_t(gid));
  if r < 0 { errno = GetErrno() }
  return;
}

func Fchown(fd int, uid int, gid int) (errno int) {
  r := libc_fchown(fd, Uid_t(uid), Gid_t(gid));
  if r < 0 { errno = GetErrno() }
  return;
}

func Truncate(path string, length int64) (errno int) {
  r := libc_truncate(StringBytePtr(path), Offset_t(length));
  if r < 0 { errno = GetErrno() }
  return;
}

func Ftruncate(fd int, length int64) (errno int) {
  r := libc_ftruncate(fd, Offset_t(length));
  if r < 0 { errno = GetErrno() }
  return;
}

func Utimes(path string, tv []Timeval) (errno int) {
  if len(tv) != 2 {
    return EINVAL;
  }
  r := libc_utimes(StringBytePtr(path),
		   (*[2]Timeval)(unsafe.Pointer(&tv[0])));
  if r < 0 {
    errno = GetErrno();
  }
  return;
}

// Getuid returns the numeric user id of the caller.
func Getuid() int {
  return int(libc_getuid());
}

// Geteuid returns the numeric effective user id of the caller.
func Geteuid() int {
  return int(libc_geteuid());
}

// Getgid returns the numeric group id of the caller.
func Getgid() int {
  return int(libc_getgid());
}

// Getegid returns the numeric effective group id of the caller.
func Getegid() int {
  return int(libc_getegid());
}

// Getgroups returns a list of the numeric ids of groups that the caller belongs to.
func Getgroups() (gids []int, errno int) {
	n := libc_getgroups(0, nil);
	if n < 0 {
		return nil, GetErrno();
	}
	if n == 0 {
		return nil, 0;
	}

	// Sanity check group count.  Max is 1<<16 on Linux.
	if n < 0 || n > 1<<20 {
		return nil, EINVAL;
	}

	a := make([]Gid_t, n);
	n = libc_getgroups(n, &a[0]);
	if n < 0 {
		return nil, GetErrno();
	}
	gids = make([]int, n);
	for i, v := range a[0:n] {
		gids[i] = int(v);
	}
	return;
}

func Getpagesize() int {
	return libc_getpagesize();
}

func TimespecToNsec(ts Timespec) int64 {
	return int64(ts.Sec)*1e9 + int64(ts.Nsec);
}

func NsecToTimespec(nsec int64) (ts Timespec) {
	ts.Sec = Timespec_sec_t(nsec / 1e9);
	ts.Nsec = Timespec_nsec_t(nsec % 1e9);
	return;
}

func TimevalToNsec(tv Timeval) int64 {
	return int64(tv.Sec)*1e9 + int64(tv.Usec)*1e3;
}

func NsecToTimeval(nsec int64) (tv Timeval) {
	nsec += 999;	// round up to microsecond
	tv.Sec = Timeval_sec_t(nsec/1e9);
	tv.Usec = Timeval_usec_t(nsec%1e9 / 1e3);
	return;
}

func Exit(code int) {
	libc_exit(code);
}

func fcntl(fd, cmd, arg int) (val int, errno int) {
  val = libc_fcntl(int(fd), int(cmd), int(arg));
  if val == -1 { errno = GetErrno() }
  return;
}

func Getpid() (pid int) {
  return int(libc_getpid());
}

func Getppid() (ppid int) {
  return int(libc_getppid());
}

func Kill(pid int, sig int) (errno int) {
	r := libc_kill(Pid_t(pid), sig)
	if r < 0 {
		errno = GetErrno()
	}
	return
}
