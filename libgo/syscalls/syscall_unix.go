// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

var (
	Stdin  = 0
	Stdout = 1
	Stderr = 2
)

const ENONE = 0

func GetErrno() int
func SetErrno(int)

func libc_uname(buf *Utsname) (errno int) __asm__("uname")

func Uname(buf *Utsname) (errno int) {
	r := libc_uname(buf)
	if r < 0 {
		errno = GetErrno()
	}
	return
}
