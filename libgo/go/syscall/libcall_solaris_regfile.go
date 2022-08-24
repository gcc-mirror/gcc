// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build (solaris && amd64) || (solaris && sparc64)
// +build solaris,amd64 solaris,sparc64

package syscall

//sys Getdents(fd int, buf []byte) (n int, err error)
//getdents(fd _C_int, buf *byte, nbyte Size_t) _C_int

func ReadDirent(fd int, buf []byte) (n int, err error) {
	return Getdents(fd, buf)
}
