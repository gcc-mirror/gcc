// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix darwin dragonfly freebsd !android,linux netbsd openbsd solaris
// +build cgo,!osusergo

package user

import (
	"fmt"
	"strconv"
	"strings"
	"syscall"
	"unsafe"
)

// bytePtrToString takes a NUL-terminated array of bytes and convert
// it to a Go string.
func bytePtrToString(p *byte) string {
	if p == nil {
		return ""
	}
	a := (*[10000]byte)(unsafe.Pointer(p))
	i := 0
	for a[i] != 0 {
		i++
	}
	return string(a[:i])
}

func current() (*User, error) {
	return lookupUnixUid(syscall.Getuid())
}

func lookupUser(username string) (*User, error) {
	var pwd syscall.Passwd
	var result *syscall.Passwd
	p := syscall.StringBytePtr(username)

	buf := alloc(userBuffer)
	defer buf.free()

	err := retryWithBuffer(buf, func() syscall.Errno {
		syscall.Entersyscall()
		rv := libc_getpwnam_r(p,
			&pwd,
			buf.ptr,
			buf.size,
			&result)
		syscall.Exitsyscall()
		if rv != 0 {
			return syscall.GetErrno()
		}
		return 0
	})
	if err != nil {
		return nil, fmt.Errorf("user: lookup username %s: %v", username, err)
	}
	if result == nil {
		return nil, UnknownUserError(username)
	}
	return buildUser(&pwd), err
}

func lookupUserId(uid string) (*User, error) {
	i, e := strconv.Atoi(uid)
	if e != nil {
		return nil, e
	}
	return lookupUnixUid(i)
}

func lookupUnixUid(uid int) (*User, error) {
	var pwd syscall.Passwd
	var result *syscall.Passwd

	buf := alloc(userBuffer)
	defer buf.free()

	err := retryWithBuffer(buf, func() syscall.Errno {
		syscall.Entersyscall()
		rv := libc_getpwuid_r(syscall.Uid_t(uid),
			&pwd,
			buf.ptr,
			buf.size,
			&result)
		syscall.Exitsyscall()
		if rv != 0 {
			return syscall.GetErrno()
		}
		return 0
	})
	if err != nil {
		return nil, fmt.Errorf("user: lookup userid %d: %v", uid, err)
	}
	if result == nil {
		return nil, UnknownUserIdError(uid)
	}
	return buildUser(&pwd), nil
}

func buildUser(pwd *syscall.Passwd) *User {
	u := &User{
		Uid:      strconv.FormatUint(uint64(pwd.Pw_uid), 10),
		Gid:      strconv.FormatUint(uint64(pwd.Pw_gid), 10),
		Username: bytePtrToString((*byte)(unsafe.Pointer(pwd.Pw_name))),
		Name:     bytePtrToString((*byte)(unsafe.Pointer(pwd.Pw_gecos))),
		HomeDir:  bytePtrToString((*byte)(unsafe.Pointer(pwd.Pw_dir))),
	}
	// The pw_gecos field isn't quite standardized. Some docs
	// say: "It is expected to be a comma separated list of
	// personal data where the first item is the full name of the
	// user."
	if i := strings.Index(u.Name, ","); i >= 0 {
		u.Name = u.Name[:i]
	}
	return u
}

func currentGroup() (*Group, error) {
	return lookupUnixGid(syscall.Getgid())
}

func lookupGroup(groupname string) (*Group, error) {
	var grp syscall.Group
	var result *syscall.Group

	buf := alloc(groupBuffer)
	defer buf.free()
	p := syscall.StringBytePtr(groupname)

	err := retryWithBuffer(buf, func() syscall.Errno {
		syscall.Entersyscall()
		rv := libc_getgrnam_r(p,
			&grp,
			buf.ptr,
			buf.size,
			&result)
		syscall.Exitsyscall()
		if rv != 0 {
			return syscall.GetErrno()
		}
		return 0
	})
	if err != nil {
		return nil, fmt.Errorf("user: lookup groupname %s: %v", groupname, err)
	}
	if result == nil {
		return nil, UnknownGroupError(groupname)
	}
	return buildGroup(&grp), nil
}

func lookupGroupId(gid string) (*Group, error) {
	i, e := strconv.Atoi(gid)
	if e != nil {
		return nil, e
	}
	return lookupUnixGid(i)
}

func lookupUnixGid(gid int) (*Group, error) {
	var grp syscall.Group
	var result *syscall.Group

	buf := alloc(groupBuffer)
	defer buf.free()

	err := retryWithBuffer(buf, func() syscall.Errno {
		syscall.Entersyscall()
		rv := libc_getgrgid_r(syscall.Gid_t(gid),
			&grp,
			buf.ptr,
			buf.size,
			&result)
		syscall.Exitsyscall()
		if rv != 0 {
			return syscall.GetErrno()
		}
		return 0
	})
	if err != nil {
		return nil, fmt.Errorf("user: lookup groupid %d: %v", gid, err)
	}
	if result == nil {
		return nil, UnknownGroupIdError(strconv.Itoa(gid))
	}
	return buildGroup(&grp), nil
}

func buildGroup(grp *syscall.Group) *Group {
	g := &Group{
		Gid:  strconv.Itoa(int(grp.Gr_gid)),
		Name: bytePtrToString((*byte)(unsafe.Pointer(grp.Gr_name))),
	}
	return g
}

type bufferKind int

const (
	userBuffer  = bufferKind(syscall.SC_GETPW_R_SIZE_MAX)
	groupBuffer = bufferKind(syscall.SC_GETGR_R_SIZE_MAX)
)

func (k bufferKind) initialSize() syscall.Size_t {
	sz, _ := syscall.Sysconf(int(k))
	if sz == -1 {
		// DragonFly and FreeBSD do not have _SC_GETPW_R_SIZE_MAX.
		// Additionally, not all Linux systems have it, either. For
		// example, the musl libc returns -1.
		return 1024
	}
	if !isSizeReasonable(int64(sz)) {
		// Truncate.  If this truly isn't enough, retryWithBuffer will error on the first run.
		return maxBufferSize
	}
	return syscall.Size_t(sz)
}

type memBuffer struct {
	ptr  *byte
	size syscall.Size_t
}

func alloc(kind bufferKind) *memBuffer {
	sz := kind.initialSize()
	b := make([]byte, sz)
	return &memBuffer{
		ptr:  &b[0],
		size: sz,
	}
}

func (mb *memBuffer) resize(newSize syscall.Size_t) {
	b := make([]byte, newSize)
	mb.ptr = &b[0]
	mb.size = newSize
}

func (mb *memBuffer) free() {
	mb.ptr = nil
}

// retryWithBuffer repeatedly calls f(), increasing the size of the
// buffer each time, until f succeeds, fails with a non-ERANGE error,
// or the buffer exceeds a reasonable limit.
func retryWithBuffer(buf *memBuffer, f func() syscall.Errno) error {
	for {
		errno := f()
		if errno == 0 {
			return nil
		} else if errno != syscall.ERANGE {
			return errno
		}
		newSize := buf.size * 2
		if !isSizeReasonable(int64(newSize)) {
			return fmt.Errorf("internal buffer exceeds %d bytes", maxBufferSize)
		}
		buf.resize(newSize)
	}
}

const maxBufferSize = 1 << 20

func isSizeReasonable(sz int64) bool {
	return sz > 0 && sz <= maxBufferSize
}

// Because we can't use cgo in tests:
func structPasswdForNegativeTest() syscall.Passwd {
	sp := syscall.Passwd{}
	sp.Pw_uid = 1<<32 - 2
	sp.Pw_gid = 1<<32 - 3
	return sp
}
