// errstr.go -- Error strings.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

//sysnb	strerror_r(errnum int, buf []byte) (errno int)
//strerror_r(errnum int, buf *byte, buflen Size_t) int

func Errstr(errnum int) string {
	for len := 128; ; len *= 2 {
		b := make([]byte, len)
		err := strerror_r(errnum, b)
		if err == 0 {
			i := 0
			for b[i] != 0 {
				i++
			}
			return string(b[:i])
		}
		if err != ERANGE {
			return "Errstr failure"
		}
	}
}
