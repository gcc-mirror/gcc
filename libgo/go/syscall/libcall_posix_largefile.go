// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// POSIX library calls on systems which use the largefile interface.

package syscall

//sys	Fstat(fd int, stat *Stat_t) (errno int)
//fstat64(fd int, stat *Stat_t) int

//sys	Ftruncate(fd int, length int64) (errno int)
//ftruncate64(fd int, length Offset_t) int

//sys	Lstat(path string, stat *Stat_t) (errno int)
//lstat64(path *byte, stat *Stat_t) int

//sys	mmap(addr uintptr, length uintptr, prot int, flags int, fd int, offset int64) (xaddr uintptr, errno int)
//mmap64(addr *byte, length Size_t, prot int, flags int, fd int, offset Offset_t) *byte

//sys	Open(path string, mode int, perm uint32) (fd int, errno int)
//open64(path *byte, mode int, perm Mode_t) int

//sys	Pread(fd int, p []byte, offset int64) (n int, errno int)
//pread64(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t

//sys	Pwrite(fd int, p []byte, offset int64) (n int, errno int)
//pwrite64(fd int, buf *byte, count Size_t, offset Offset_t) Ssize_t

//sys	Seek(fd int, offset int64, whence int) (off int64, errno int)
//lseek64(fd int, offset Offset_t, whence int) Offset_t

//sys	Stat(path string, stat *Stat_t) (errno int)
//stat64(path *byte, stat *Stat_t) int

//sys	Truncate(path string, length int64) (errno int)
//truncate64(path *byte, length Offset_t) int
