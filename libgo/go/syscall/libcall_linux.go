// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// GNU/Linux library calls.

package syscall

import "unsafe"

//sys	Openat(dirfd int, path string, flags int, mode uint32) (fd int, errno int)
//openat(dirfd int, path *byte, flags int, mode Mode_t) int

//sys	futimesat(dirfd int, path *byte, times *[2]Timeval) (errno int)
//futimesat(dirfd int, path *byte, times *[2]Timeval) int
func Futimesat(dirfd int, path string, tv []Timeval) (errno int) {
	if len(tv) != 2 {
		return EINVAL
	}
	return futimesat(dirfd, StringBytePtr(path), (*[2]Timeval)(unsafe.Pointer(&tv[0])))
}

func Futimes(fd int, tv []Timeval) (errno int) {
	// Believe it or not, this is the best we can do on GNU/Linux
	// (and is what glibc does).
	return Utimes("/proc/self/fd/"+itoa(fd), tv)
}

//sys	ptrace(request int, pid int, addr uintptr, data uintptr) (errno int)
//ptrace(request int, pid Pid_t, addr *byte, data *byte) _C_long

//sysnb raw_ptrace(request int, pid int, addr *byte, data *byte) (errno int)
//ptrace(request int, pid Pid_t, addr *byte, data *byte) _C_long

func ptracePeek(req int, pid int, addr uintptr, out []byte) (count int, errno int) {
	// The peek requests are machine-size oriented, so we wrap it
	// to retrieve arbitrary-length data.

	// The ptrace syscall differs from glibc's ptrace.
	// Peeks returns the word in *data, not as the return value.

	var buf [sizeofPtr]byte

	// Leading edge.  PEEKTEXT/PEEKDATA don't require aligned
	// access (PEEKUSER warns that it might), but if we don't
	// align our reads, we might straddle an unmapped page
	// boundary and not get the bytes leading up to the page
	// boundary.
	n := 0
	if addr%sizeofPtr != 0 {
		errno = ptrace(req, pid, addr-addr%sizeofPtr, uintptr(unsafe.Pointer(&buf[0])))
		if errno != 0 {
			return 0, errno
		}
		n += copy(out, buf[addr%sizeofPtr:])
		out = out[n:]
	}

	// Remainder.
	for len(out) > 0 {
		// We use an internal buffer to gaurantee alignment.
		// It's not documented if this is necessary, but we're paranoid.
		errno = ptrace(req, pid, addr+uintptr(n), uintptr(unsafe.Pointer(&buf[0])))
		if errno != 0 {
			return n, errno
		}
		copied := copy(out, buf[0:])
		n += copied
		out = out[copied:]
	}

	return n, 0
}

func PtracePeekText(pid int, addr uintptr, out []byte) (count int, errno int) {
	return ptracePeek(PTRACE_PEEKTEXT, pid, addr, out)
}

func PtracePeekData(pid int, addr uintptr, out []byte) (count int, errno int) {
	return ptracePeek(PTRACE_PEEKDATA, pid, addr, out)
}

func ptracePoke(pokeReq int, peekReq int, pid int, addr uintptr, data []byte) (count int, errno int) {
	// As for ptracePeek, we need to align our accesses to deal
	// with the possibility of straddling an invalid page.

	// Leading edge.
	n := 0
	if addr%sizeofPtr != 0 {
		var buf [sizeofPtr]byte
		errno = ptrace(peekReq, pid, addr-addr%sizeofPtr, uintptr(unsafe.Pointer(&buf[0])))
		if errno != 0 {
			return 0, errno
		}
		n += copy(buf[addr%sizeofPtr:], data)
		word := *((*uintptr)(unsafe.Pointer(&buf[0])))
		errno = ptrace(pokeReq, pid, addr-addr%sizeofPtr, word)
		if errno != 0 {
			return 0, errno
		}
		data = data[n:]
	}

	// Interior.
	for len(data) > int(sizeofPtr) {
		word := *((*uintptr)(unsafe.Pointer(&data[0])))
		errno = ptrace(pokeReq, pid, addr+uintptr(n), word)
		if errno != 0 {
			return n, errno
		}
		n += int(sizeofPtr)
		data = data[sizeofPtr:]
	}

	// Trailing edge.
	if len(data) > 0 {
		var buf [sizeofPtr]byte
		errno = ptrace(peekReq, pid, addr+uintptr(n), uintptr(unsafe.Pointer(&buf[0])))
		if errno != 0 {
			return n, errno
		}
		copy(buf[0:], data)
		word := *((*uintptr)(unsafe.Pointer(&buf[0])))
		errno = ptrace(pokeReq, pid, addr+uintptr(n), word)
		if errno != 0 {
			return n, errno
		}
		n += len(data)
	}

	return n, 0
}

func PtracePokeText(pid int, addr uintptr, data []byte) (count int, errno int) {
	return ptracePoke(PTRACE_POKETEXT, PTRACE_PEEKTEXT, pid, addr, data)
}

func PtracePokeData(pid int, addr uintptr, data []byte) (count int, errno int) {
	return ptracePoke(PTRACE_POKEDATA, PTRACE_PEEKDATA, pid, addr, data)
}

func PtraceGetRegs(pid int, regsout *PtraceRegs) (errno int) {
	return ptrace(PTRACE_GETREGS, pid, 0, uintptr(unsafe.Pointer(regsout)))
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (errno int) {
	return ptrace(PTRACE_SETREGS, pid, 0, uintptr(unsafe.Pointer(regs)))
}

func PtraceSetOptions(pid int, options int) (errno int) {
	return ptrace(PTRACE_SETOPTIONS, pid, 0, uintptr(options))
}

func PtraceGetEventMsg(pid int) (msg uint, errno int) {
	var data _C_long
	errno = ptrace(PTRACE_GETEVENTMSG, pid, 0, uintptr(unsafe.Pointer(&data)))
	msg = uint(data)
	return
}

func PtraceCont(pid int, signal int) (errno int) {
	return ptrace(PTRACE_CONT, pid, 0, uintptr(signal))
}

func PtraceSingleStep(pid int) (errno int) { return ptrace(PTRACE_SINGLESTEP, pid, 0, 0) }

func PtraceAttach(pid int) (errno int) { return ptrace(PTRACE_ATTACH, pid, 0, 0) }

func PtraceDetach(pid int) (errno int) { return ptrace(PTRACE_DETACH, pid, 0, 0) }

// FIXME: mksysinfo needs to produce LINUX_REBOOT_MAGIC[12].

// //sys	reboot(magic1 uint, magic2 uint, cmd int, arg string) (errno int)
// //reboot(magic1 uint, magic2 uint, cmd int, arg *byte) int
// func Reboot(cmd int) (errno int) {
// 	return reboot(LINUX_REBOOT_MAGIC1, LINUX_REBOOT_MAGIC2, cmd, "")
// }

//sys	Acct(path string) (errno int)
//acct(path *byte) int

// FIXME: mksysinfo Timex
// //sys	Adjtimex(buf *Timex) (state int, errno int)
// //adjtimex(buf *Timex) int

//sys	Faccessat(dirfd int, path string, mode uint32, flags int) (errno int)
//faccessat(dirfd int, pathname *byte, mode int, flags int) int

// FIXME: Only in glibc 2.10 and later.
// //sys	Fallocate(fd int, mode uint32, off int64, len int64) (errno int)
// //fallocate(fd int, mode int, offset Offset_t, len Offset_t) int

//sys	Fchmodat(dirfd int, path string, mode uint32, flags int) (errno int)
//fchmodat(dirfd int, pathname *byte, mode Mode_t, flags int) int

//sys	Fchownat(dirfd int, path string, uid int, gid int, flags int) (errno int)
//fchownat(dirfd int, path *byte, owner Uid_t, group Gid_t, flags int) int

//sys	Flock(fd int, how int) (errno int)
//flock(fd int, how int) int

// FIXME: mksysinfo statfs
// //sys	Fstatfs(fd int, buf *Statfs_t) (errno int)
// //fstatfs(fd int, buf *Statfs_t) int

// FIXME: Only available as a syscall.
// //sysnb	Gettid() (tid int)
// //gettid() Pid_t

//sys	Ioperm(from int, num int, on int) (errno int)
//ioperm(from _C_long, num _C_long, on int) int

//sys	Iopl(level int) (errno int)
//iopl(level int) int

// FIXME: mksysinfo linux_dirent
//    Or just abandon this function.
// //sys	Getdents(fd int, buf []byte) (n int, errno int)
// //getdents64(fd int, buf *byte, count uint)

//sys	InotifyAddWatch(fd int, pathname string, mask uint32) (watchdesc int, errno int)
//inotify_add_watch(fd int, pathname *byte, mask uint32) int

//sysnb	InotifyInit() (fd int, errno int)
//inotify_init() int

// FIXME: Only in glibc 2.9 and later.
// //sysnb	InotifyInit1(flags int) (fd int, errno int)
// //inotify_init1(flags int) int

//sysnb	InotifyRmWatch(fd int, watchdesc uint32) (success int, errno int)
//inotify_rm_watch(fd int, wd uint32) int

//sys	Klogctl(typ int, buf []byte) (n int, errno int)
//klogctl(typ int, bufp *byte, len int) int

//sys	Mkdirat(dirfd int, path string, mode uint32) (errno int)
//mkdirat(dirfd int, path *byte, mode Mode_t) int

//sys	Mknodat(dirfd int, path string, mode uint32, dev int) (errno int)
//mknodat(dirfd int, path *byte, mode Mode_t, dev _dev_t) int

//sys	PivotRoot(newroot string, putold string) (errno int)
//pivot_root(newroot *byte, putold *byte) int

//sys	Renameat(olddirfd int, oldpath string, newdirfd int, newpath string) (errno int)
//renameat(olddirfd int, oldpath *byte, newdirfd int, newpath *byte) int

//sys	sendfile(outfd int, infd int, offset *Offset_t, count int) (written int, errno int)
//sendfile64(outfd int, infd int, offset *Offset_t, count Size_t) Ssize_t
func Sendfile(outfd int, infd int, offset *int64, count int) (written int, errno int) {
	var soff Offset_t
	var psoff *Offset_t
	if offset != nil {
		psoff = &soff
	}
	written, errno = sendfile(outfd, infd, psoff, count)
	if offset != nil {
		*offset = int64(soff)
	}
	return
}

//sys	Setfsgid(gid int) (errno int)
//setfsgid(gid Gid_t) int

//sys	Setfsuid(uid int) (errno int)
//setfsuid(uid Uid_t) int

//sysnb	Setresgid(rgid int, egid int, sgid int) (errno int)
//setresgid(rgid Gid_t, egid Gid_t, sgid Gid_t) int

//sysnb	Setresuid(ruid int, eguid int, suid int) (errno int)
//setresuid(ruid Uid_t, euid Uid_t, suid Uid_t) int

//sys	splice(rfd int, roff *_loff_t, wfd int, woff *_loff_t, len int, flags int) (n int64, errno int)
//splice(rfd int, roff *_loff_t, wfd int, woff *_loff_t, len Size_t, flags uint) Ssize_t
func Splice(rfd int, roff *int64, wfd int, woff *int64, len int, flags int) (n int64, errno int) {
	var lroff _loff_t
	var plroff *_loff_t
	if (roff != nil) {
		plroff = &lroff
	}
	var lwoff _loff_t
	var plwoff *_loff_t
	if (woff != nil) {
		plwoff = &lwoff
	}
	n, errno = splice(rfd, plroff, wfd, plwoff, len, flags)
	if (roff != nil) {
		*roff = int64(lroff)
	}
	if (woff != nil) {
		*woff = int64(lwoff)
	}
	return
}

// FIXME: mksysinfo statfs
// //sys	Statfs(path string, buf *Statfs_t) (errno int)
// //statfs(path *byte, buf *Statfs_t) int

// FIXME: Only in glibc 2.6 and later.
// //sys	SyncFileRange(fd int, off int64, n int64, flags int) (errno int)
// //sync_file_range(fd int, off Offset_t, n Offset_t, flags uint) int

// FIXME: mksysinfo Sysinfo_t
// //sysnb	Sysinfo(info *Sysinfo_t) (errno int)
// //sysinfo(info *Sysinfo_t) int

//sys	Tee(rfd int, wfd int, len int, flags int) (n int64, errno int)
//tee(rfd int, wfd int, len Size_t, flags uint) Ssize_t

// FIXME: Only available as a syscall.
// //sysnb	Tgkill(tgid int, tid int, sig int) (errno int)
// //tgkill(tgid int, tid int, sig int) int

//sys	unlinkat(dirfd int, path string, flags int) (errno int)
//unlinkat(dirfd int, path *byte, flags int) int

func Unlinkat(dirfd int, path string) (errno int) {
	return unlinkat(dirfd, path, 0)
}

//sys	Unmount(target string, flags int) (errno int) = SYS_UMOUNT2
//umount2(target *byte, flags int) int

//sys	Unshare(flags int) (errno int)
//unshare(flags int) int

// FIXME: mksysinfo Ustat_t
// //sys	Ustat(dev int, ubuf *Ustat_t) (errno int)
// //ustat(dev _dev_t, ubuf *Ustat_t) int
