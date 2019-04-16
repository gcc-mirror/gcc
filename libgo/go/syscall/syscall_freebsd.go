// Copyright 2009,2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import (
	"sync"
	"unsafe"
)

const (
	_SYS_FSTAT_FREEBSD12         = 551 // { int fstat(int fd, _Out_ struct stat *sb); }
	_SYS_FSTATAT_FREEBSD12       = 552 // { int fstatat(int fd, _In_z_ char *path, \
	_SYS_GETDIRENTRIES_FREEBSD12 = 554 // { ssize_t getdirentries(int fd, \
	_SYS_STATFS_FREEBSD12        = 555 // { int statfs(_In_z_ char *path, \
	_SYS_FSTATFS_FREEBSD12       = 556 // { int fstatfs(int fd, \
	_SYS_GETFSSTAT_FREEBSD12     = 557 // { int getfsstat( \
	_SYS_MKNODAT_FREEBSD12       = 559 // { int mknodat(int fd, _In_z_ char *path, \
)

// See https://www.freebsd.org/doc/en_US.ISO8859-1/books/porters-handbook/versions.html.
var (
	osreldateOnce sync.Once
	osreldate     uint32
)

// INO64_FIRST from /usr/src/lib/libc/sys/compat-ino64.h
const _ino64First = 1200031

func supportsABI(ver uint32) bool {
	osreldateOnce.Do(func() { osreldate, _ = SysctlUint32("kern.osreldate") })
	return osreldate >= ver
}

func direntIno(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Fileno), unsafe.Sizeof(Dirent{}.Fileno))
}

func direntReclen(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Reclen), unsafe.Sizeof(Dirent{}.Reclen))
}

func direntNamlen(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Namlen), unsafe.Sizeof(Dirent{}.Namlen))
}
