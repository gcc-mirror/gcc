// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix

package syscall

import (
	"unsafe"
)

//sys	Openat(dirfd int, path string, flags int, mode uint32) (fd int, err error)
//open64at(dirfd _C_int, path *byte, flags _C_int, mode Mode_t) _C_int

//sys	ptrace(request int, id int, addr uintptr, data int, buff uintptr) (val int)
//ptrace(request _C_int, id int, addr uintptr, data _C_int, buff *byte) _C_int

//sys	ptrace64(request int, id int64, addr int64, data int, buff uintptr) (err error)
//ptrace64(request _C_int, id int64, addr int64, data _C_int, buff *byte) _C_int

func raw_ptrace(request int, pid int, addr *byte, data *byte) Errno {
	if request == _PTRACE_TRACEME {
		// Convert to AIX ptrace call.
		err := ptrace64(_PT_TRACE_ME, 0, 0, 0, 0)
		if err != nil {
			return err.(Errno)
		}
		return 0
	}
	return ENOSYS
}

func ptracePeek(pid int, addr uintptr, out []byte) (count int, err error) {
	n := 0
	for len(out) > 0 {
		bsize := len(out)
		if bsize > 1024 {
			bsize = 1024
		}
		err = ptrace64(_PT_READ_BLOCK, int64(pid), int64(addr), bsize, uintptr(unsafe.Pointer(&out[0])))
		if err != nil {
			return 0, err
		}
		addr += uintptr(bsize)
		n += bsize
		out = out[n:]
	}
	return n, nil
}

func PtracePeekText(pid int, addr uintptr, out []byte) (count int, err error) {
	return ptracePeek(pid, addr, out)
}

func PtracePeekData(pid int, addr uintptr, out []byte) (count int, err error) {
	return ptracePeek(pid, addr, out)
}

func ptracePoke(pid int, addr uintptr, data []byte) (count int, err error) {
	n := 0
	for len(data) > 0 {
		bsize := len(data)
		if bsize > 1024 {
			bsize = 1024
		}
		err = ptrace64(_PT_WRITE_BLOCK, int64(pid), int64(addr), bsize, uintptr(unsafe.Pointer(&data[0])))
		if err != nil {
			return 0, err
		}
		addr += uintptr(bsize)
		n += bsize
		data = data[n:]
	}
	return n, nil
}

func PtracePokeText(pid int, addr uintptr, data []byte) (count int, err error) {
	return ptracePoke(pid, addr, data)
}

func PtracePokeData(pid int, addr uintptr, data []byte) (count int, err error) {
	return ptracePoke(pid, addr, data)
}

func PtraceCont(pid int, signal int) (err error) {
	return ptrace64(_PT_CONTINUE, int64(pid), 1, signal, 0)
}

func PtraceSingleStep(pid int) (err error) { return ptrace64(_PT_STEP, int64(pid), 1, 0, 0) }

func PtraceAttach(pid int) (err error) { return ptrace64(_PT_ATTACH, int64(pid), 0, 0, 0) }

func PtraceDetach(pid int) (err error) { return ptrace64(_PT_DETACH, int64(pid), 0, 0, 0) }

//sys	reboot(how int) (err error)
//__linux_reboot(how _C_int) _C_int
func Reboot(how int) (err error) {
	return reboot(how)
}

//sys	Acct(path string) (err error)
//acct(path *byte) _C_int

//sys	Faccessat(dirfd int, path string, mode uint32, flags int) (err error)
//faccessat(dirfd _C_int, pathname *byte, mode _C_int, flags _C_int) _C_int

//sys	Fchmodat(dirfd int, path string, mode uint32, flags int) (err error)
//fchmodat(dirfd _C_int, pathname *byte, mode Mode_t, flags _C_int) _C_int

//sys	Fchownat(dirfd int, path string, uid int, gid int, flags int) (err error)
//fchownat(dirfd _C_int, path *byte, owner Uid_t, group Gid_t, flags _C_int) _C_int

//sys	Fstatfs(fd int, buf *Statfs_t) (err error)
//fstatfs64(fd _C_int, buf *Statfs_t) _C_int

//sys	Mkdirat(dirfd int, path string, mode uint32) (err error)
//mkdirat(dirfd _C_int, path *byte, mode Mode_t) _C_int

//sys	Mknodat(dirfd int, path string, mode uint32, dev int) (err error)
//mknodat(dirfd _C_int, path *byte, mode Mode_t, dev _dev_t) _C_int

//sys	getdirent(fd int, buf []byte) (n int, err error)
//getdirent64(fd _C_int, buf *byte, nbyte Size_t) _C_int

func ReadDirent(fd int, buf []byte) (n int, err error) {
	return getdirent(fd, buf)
}

//sys	Renameat(olddirfd int, oldpath string, newdirfd int, newpath string) (err error)
//renameat(olddirfd _C_int, oldpath *byte, newdirfd _C_int, newpath *byte) _C_int

//sys	Statfs(path string, buf *Statfs_t) (err error)
//statfs64(path *byte, buf *Statfs_t) _C_int

//sys	unlinkat(dirfd int, path string, flags int) (err error)
//unlinkat(dirfd _C_int, path *byte, flags _C_int) _C_int

func Unlinkat(dirfd int, path string) (err error) {
	return unlinkat(dirfd, path, 0)
}
