// errstr_rtems.go -- RTEMS specific error strings.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func Errstr(errno int) string {
	for len := Size_t(128); ; len *= 2 {
		b := make([]byte, len+1)

		// The newlib strerror_r always returns the string in buffer.
		libc_strerror_r(errno, &b[0], len)
		b[len] = 0

		i := 0
		for b[i] != 0 {
			i++
		}

		if Size_t(i) < len {
			return string(b[0:i])
		}
	}
}
