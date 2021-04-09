// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package os

import (
	"internal/poll"
	"io"
	"syscall"
	"unsafe"
)

// FIXME: pathconf returns long, not int.
//extern pathconf
func libc_pathconf(*byte, int32) int

func direntType(*syscall.Dirent) byte

func clen(n []byte) int {
	for i := 0; i < len(n); i++ {
		if n[i] == 0 {
			return i
		}
	}
	return len(n)
}

func (f *File) readdir(n int, mode readdirMode) (names []string, dirents []DirEntry, infos []FileInfo, err error) {
	// If this file has no dirinfo, create one.
	if f.dirinfo == nil {
		fd, call, err := poll.DupCloseOnExec(int(f.pfd.Sysfd))
		if err != nil {
			return nil, nil, nil, NewSyscallError(call, err)
		}

		syscall.Entersyscall()
		r := libc_fdopendir(int32(fd))
		errno := syscall.GetErrno()
		syscall.Exitsyscall()
		if r == nil {
			return nil, nil, nil, &PathError{"fdopendir", f.name, errno}
		}

		f.dirinfo = &dirInfo{r}
	}
	dir := f.dirinfo.dir

	// Change the meaning of n for the implementation below.
	//
	// The n above was for the public interface of "if n <= 0,
	// Readdir returns all the FileInfo from the directory in a
	// single slice".
	//
	// But below, we use only negative to mean looping until the
	// end and positive to mean bounded, with positive
	// terminating at 0.
	if n == 0 {
		n = -1
	}

	for n != 0 {
		syscall.Entersyscall()
		syscall.SetErrno(0)
		dirent := libc_readdir(dir)
		errno := syscall.GetErrno()
		syscall.Exitsyscall()

		if dirent == nil {
			if errno != 0 {
				return names, dirents, infos, &PathError{Op: "readdir", Path: f.name, Err: errno}
			}
			break // EOF
		}

		// In some cases the actual name can be longer than
		// the Name field.
		name := (*[1 << 16]byte)(unsafe.Pointer(&dirent.Name[0]))[:]
		for i, c := range name {
			if c == 0 {
				name = name[:i]
				break
			}
		}
		// Check for useless names before allocating a string.
		if (len(name) == 1 && name[0] == '.') || (len(name) == 2 && name[0] == '.' && name[1] == '.') {
			continue
		}
		if n > 0 { // see 'n == 0' comment above
			n--
		}
		if mode == readdirName {
			names = append(names, string(name))
		} else if mode == readdirDirEntry {
			var typ FileMode
			switch direntType(dirent) {
			case 'B':
				typ = ModeDevice
			case 'C':
				typ = ModeDevice | ModeCharDevice
			case 'D':
				typ = ModeDir
			case 'F':
				typ = ModeNamedPipe
			case 'L':
				typ = ModeSymlink
			case 'R':
				typ = 0
			case 'S':
				typ = ModeSocket
			case 'U':
				typ = ^FileMode(0)
			}

			de, err := newUnixDirent(f.name, string(name), typ)
			if IsNotExist(err) {
				// File disappeared between readdir and stat.
				// Treat as if it didn't exist.
				continue
			}
			if err != nil {
				return nil, dirents, nil, err
			}
			dirents = append(dirents, de)
		} else {
			info, err := lstat(f.name + "/" + string(name))
			if IsNotExist(err) {
				// File disappeared between readdir + stat.
				// Treat as if it didn't exist.
				continue
			}
			if err != nil {
				return nil, nil, infos, err
			}
			infos = append(infos, info)
		}
	}

	if n > 0 && len(names)+len(dirents)+len(infos) == 0 {
		return nil, nil, nil, io.EOF
	}
	return names, dirents, infos, nil
}
