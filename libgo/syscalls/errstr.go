// errstr.go -- Error strings.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func Errstr(errno int) string {
	for len := Size_t(128); ; len *= 2 {
		b := make([]byte, len)
		r := libc_strerror_r(errno, &b[0], len)
		if r >= 0 {
			i := 0
			for b[i] != 0 {
				i++
			}
			return string(b[:i])
		}
		if GetErrno() != ERANGE {
			return "Errstr failure"
		}
	}
}
