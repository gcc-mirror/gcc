// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build solaris,386 solaris,sparc

package syscall

//sys Getdents(fd int, buf []byte) (n int, err error)
//getdents64(fd _C_int, buf *byte, nbyte Size_t) _C_int

func ReadDirent(fd int, buf []byte) (n int, err error) {
	return Getdents(fd, buf)
}
