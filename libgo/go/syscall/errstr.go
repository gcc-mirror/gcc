// errstr.go -- Error strings.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "internal/bytealg"

//extern go_strerror
func go_strerror(_C_int, *byte, Size_t) _C_int

func Errstr(errnum int) string {
	for size := 128; ; size *= 2 {
		b := make([]byte, size)
		errno := go_strerror(_C_int(errnum), &b[0], Size_t(len(b)))
		if errno == 0 {
			i := bytealg.IndexByte(b, 0)
			// Lowercase first letter: Bad -> bad, but
			// STREAM -> STREAM.
			if i > 1 && 'A' <= b[0] && b[0] <= 'Z' && 'a' <= b[1] && b[1] <= 'z' {
				b[0] += 'a' - 'A'
			}
			return string(b[:i])
		}
		if errno != ERANGE {
			return "strerror_r failure"
		}
	}
}
