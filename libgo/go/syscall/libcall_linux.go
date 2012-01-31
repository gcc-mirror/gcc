// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// GNU/Linux library calls.

package syscall

import "unsafe"

//sys	Openat(dirfd int, path string, flags int, mode uint32) (fd int, err error)
//openat(dirfd int, path *byte, flags int, mode Mode_t) int

//sys	futimesat(dirfd int, path *byte, times *[2]Timeval) (err error)
//futimesat(dirfd int, path *byte, times *[2]Timeval) int
func Futimesat(dirfd int, path string, tv []Timeval) (err error) {
	if len(tv) != 2 {
		return EINVAL
	}
	return futimesat(dirfd, StringBytePtr(path), (*[2]Timeval)(unsafe.Pointer(&tv[0])))
}

func Futimes(fd int, tv []Timeval) (err error) {
	// Believe it or not, this is the best we can do on GNU/Linux
	// (and is what glibc does).
	return Utimes("/proc/self/fd/"+itoa(fd), tv)
}

//sys	ptrace(request int, pid int, addr uintptr, data uintptr) (err error)
//ptrace(request int, pid Pid_t, addr *byte, data *byte) _C_long

//sysnb raw_ptrace(request int, pid int, addr *byte, data *byte) (err Errno)
//ptrace(request int, pid Pid_t, addr *byte, data *byte) _C_long

func ptracePeek(req int, pid int, addr uintptr, out []byte) (count int, err error) {
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
		err = ptrace(req, pid, addr-addr%sizeofPtr, uintptr(unsafe.Pointer(&buf[0])))
		if err != nil {
			return 0, err
		}
		n += copy(out, buf[addr%sizeofPtr:])
		out = out[n:]
	}

	// Remainder.
	for len(out) > 0 {
		// We use an internal buffer to gaurantee alignment.
		// It's not documented if this is necessary, but we're paranoid.
		err = ptrace(req, pid, addr+uintptr(n), uintptr(unsafe.Pointer(&buf[0])))
		if err != nil {
			return n, err
		}
		copied := copy(out, buf[0:])
		n += copied
		out = out[copied:]
	}

	return n, nil
}

func PtracePeekText(pid int, addr uintptr, out []byte) (count int, err error) {
	return ptracePeek(PTRACE_PEEKTEXT, pid, addr, out)
}

func PtracePeekData(pid int, addr uintptr, out []byte) (count int, err error) {
	return ptracePeek(PTRACE_PEEKDATA, pid, addr, out)
}

func ptracePoke(pokeReq int, peekReq int, pid int, addr uintptr, data []byte) (count int, err error) {
	// As for ptracePeek, we need to align our accesses to deal
	// with the possibility of straddling an invalid page.

	// Leading edge.
	n := 0
	if addr%sizeofPtr != 0 {
		var buf [sizeofPtr]byte
		err = ptrace(peekReq, pid, addr-addr%sizeofPtr, uintptr(unsafe.Pointer(&buf[0])))
		if err != nil {
			return 0, err
		}
		n += copy(buf[addr%sizeofPtr:], data)
		word := *((*uintptr)(unsafe.Pointer(&buf[0])))
		err = ptrace(pokeReq, pid, addr-addr%sizeofPtr, word)
		if err != nil {
			return 0, err
		}
		data = data[n:]
	}

	// Interior.
	for len(data) > int(sizeofPtr) {
		word := *((*uintptr)(unsafe.Pointer(&data[0])))
		err = ptrace(pokeReq, pid, addr+uintptr(n), word)
		if err != nil {
			return n, err
		}
		n += int(sizeofPtr)
		data = data[sizeofPtr:]
	}

	// Trailing edge.
	if len(data) > 0 {
		var buf [sizeofPtr]byte
		err = ptrace(peekReq, pid, addr+uintptr(n), uintptr(unsafe.Pointer(&buf[0])))
		if err != nil {
			return n, err
		}
		copy(buf[0:], data)
		word := *((*uintptr)(unsafe.Pointer(&buf[0])))
		err = ptrace(pokeReq, pid, addr+uintptr(n), word)
		if err != nil {
			return n, err
		}
		n += len(data)
	}

	return n, nil
}

func PtracePokeText(pid int, addr uintptr, data []byte) (count int, err error) {
	return ptracePoke(PTRACE_POKETEXT, PTRACE_PEEKTEXT, pid, addr, data)
}

func PtracePokeData(pid int, addr uintptr, data []byte) (count int, err error) {
	return ptracePoke(PTRACE_POKEDATA, PTRACE_PEEKDATA, pid, addr, data)
}

func PtraceSetOptions(pid int, options int) (err error) {
	return ptrace(PTRACE_SETOPTIONS, pid, 0, uintptr(options))
}

func PtraceGetEventMsg(pid int) (msg uint, err error) {
	var data _C_long
	err = ptrace(PTRACE_GETEVENTMSG, pid, 0, uintptr(unsafe.Pointer(&data)))
	msg = uint(data)
	return
}

func PtraceCont(pid int, signal int) (err error) {
	return ptrace(PTRACE_CONT, pid, 0, uintptr(signal))
}

func PtraceSingleStep(pid int) (err error) { return ptrace(PTRACE_SINGLESTEP, pid, 0, 0) }

func PtraceAttach(pid int) (err error) { return ptrace(PTRACE_ATTACH, pid, 0, 0) }

func PtraceDetach(pid int) (err error) { return ptrace(PTRACE_DETACH, pid, 0, 0) }

// FIXME: mksysinfo needs to produce LINUX_REBOOT_MAGIC[12].

// //sys	reboot(magic1 uint, magic2 uint, cmd int, arg string) (err error)
// //reboot(magic1 uint, magic2 uint, cmd int, arg *byte) int
// func Reboot(cmd int) (err error) {
// 	return reboot(LINUX_REBOOT_MAGIC1, LINUX_REBOOT_MAGIC2, cmd, "")
// }

//sys	Acct(path string) (err error)
//acct(path *byte) int

// FIXME: mksysinfo Timex
// //sys	Adjtimex(buf *Timex) (state int, err error)
// //adjtimex(buf *Timex) int

//sys	Faccessat(dirfd int, path string, mode uint32, flags int) (err error)
//faccessat(dirfd int, pathname *byte, mode int, flags int) int

// FIXME: Only in glibc 2.10 and later.
// //sys	Fallocate(fd int, mode uint32, off int64, len int64) (err error)
// //fallocate(fd int, mode int, offset Offset_t, len Offset_t) int

//sys	Fchmodat(dirfd int, path string, mode uint32, flags int) (err error)
//fchmodat(dirfd int, pathname *byte, mode Mode_t, flags int) int

//sys	Fchownat(dirfd int, path string, uid int, gid int, flags int) (err error)
//fchownat(dirfd int, path *byte, owner Uid_t, group Gid_t, flags int) int

//sys	Flock(fd int, how int) (err error)
//flock(fd int, how int) int

// FIXME: mksysinfo statfs
// //sys	Fstatfs(fd int, buf *Statfs_t) (err error)
// //fstatfs(fd int, buf *Statfs_t) int

func Gettid() (tid int) {
	r1, _, _ := Syscall(SYS_GETTID, 0, 0, 0)
	return int(r1)
}

// FIXME: mksysinfo linux_dirent
//    Or just abandon this function.
// //sys	Getdents(fd int, buf []byte) (n int, err error)
// //getdents64(fd int, buf *byte, count uint)

//sys	InotifyAddWatch(fd int, pathname string, mask uint32) (watchdesc int, err error)
//inotify_add_watch(fd int, pathname *byte, mask uint32) int

//sysnb	InotifyInit() (fd int, err error)
//inotify_init() int

// FIXME: Only in glibc 2.9 and later.
// //sysnb	InotifyInit1(flags int) (fd int, err error)
// //inotify_init1(flags int) int

//sysnb	InotifyRmWatch(fd int, watchdesc uint32) (success int, err error)
//inotify_rm_watch(fd int, wd uint32) int

//sys	Klogctl(typ int, buf []byte) (n int, err error)
//klogctl(typ int, bufp *byte, len int) int

//sys	Mkdirat(dirfd int, path string, mode uint32) (err error)
//mkdirat(dirfd int, path *byte, mode Mode_t) int

//sys	Mknodat(dirfd int, path string, mode uint32, dev int) (err error)
//mknodat(dirfd int, path *byte, mode Mode_t, dev _dev_t) int

//sys	PivotRoot(newroot string, putold string) (err error)
//pivot_root(newroot *byte, putold *byte) int

//sys	Renameat(olddirfd int, oldpath string, newdirfd int, newpath string) (err error)
//renameat(olddirfd int, oldpath *byte, newdirfd int, newpath *byte) int

//sys	sendfile(outfd int, infd int, offset *Offset_t, count int) (written int, err error)
//sendfile64(outfd int, infd int, offset *Offset_t, count Size_t) Ssize_t
func Sendfile(outfd int, infd int, offset *int64, count int) (written int, err error) {
	var soff Offset_t
	var psoff *Offset_t
	if offset != nil {
		psoff = &soff
	}
	written, err = sendfile(outfd, infd, psoff, count)
	if offset != nil {
		*offset = int64(soff)
	}
	return
}

//sys	Setfsgid(gid int) (err error)
//setfsgid(gid Gid_t) int

//sys	Setfsuid(uid int) (err error)
//setfsuid(uid Uid_t) int

//sysnb	Setresgid(rgid int, egid int, sgid int) (err error)
//setresgid(rgid Gid_t, egid Gid_t, sgid Gid_t) int

//sysnb	Setresuid(ruid int, eguid int, suid int) (err error)
//setresuid(ruid Uid_t, euid Uid_t, suid Uid_t) int

//sys	splice(rfd int, roff *_loff_t, wfd int, woff *_loff_t, len int, flags int) (n int64, err error)
//splice(rfd int, roff *_loff_t, wfd int, woff *_loff_t, len Size_t, flags uint) Ssize_t
func Splice(rfd int, roff *int64, wfd int, woff *int64, len int, flags int) (n int64, err error) {
	var lroff _loff_t
	var plroff *_loff_t
	if roff != nil {
		plroff = &lroff
	}
	var lwoff _loff_t
	var plwoff *_loff_t
	if woff != nil {
		plwoff = &lwoff
	}
	n, err = splice(rfd, plroff, wfd, plwoff, len, flags)
	if roff != nil {
		*roff = int64(lroff)
	}
	if woff != nil {
		*woff = int64(lwoff)
	}
	return
}

// FIXME: mksysinfo statfs
// //sys	Statfs(path string, buf *Statfs_t) (err error)
// //statfs(path *byte, buf *Statfs_t) int

// FIXME: Only in glibc 2.6 and later.
// //sys	SyncFileRange(fd int, off int64, n int64, flags int) (err error)
// //sync_file_range(fd int, off Offset_t, n Offset_t, flags uint) int

// FIXME: mksysinfo Sysinfo_t
// //sysnb	Sysinfo(info *Sysinfo_t) (err error)
// //sysinfo(info *Sysinfo_t) int

//sys	Tee(rfd int, wfd int, len int, flags int) (n int64, err error)
//tee(rfd int, wfd int, len Size_t, flags uint) Ssize_t

// FIXME: Only available as a syscall.
// //sysnb	Tgkill(tgid int, tid int, sig int) (err error)
// //tgkill(tgid int, tid int, sig int) int

//sys	unlinkat(dirfd int, path string, flags int) (err error)
//unlinkat(dirfd int, path *byte, flags int) int

func Unlinkat(dirfd int, path string) (err error) {
	return unlinkat(dirfd, path, 0)
}

//sys	Unmount(target string, flags int) (err error) = SYS_UMOUNT2
//umount2(target *byte, flags int) int

//sys	Unshare(flags int) (err error)
//unshare(flags int) int

// FIXME: mksysinfo Ustat_t
// //sys	Ustat(dev int, ubuf *Ustat_t) (err error)
// //ustat(dev _dev_t, ubuf *Ustat_t) int
