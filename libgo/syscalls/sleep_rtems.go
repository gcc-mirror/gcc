// sleep_rtems.go -- Sleep on RTEMS.

// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

func libc_nanosleep(req *Timespec, rem *Timespec) int __asm__ ("nanosleep")

func Sleep(nsec int64) (errno int) {
	errno = 0
	ts := NsecToTimespec(nsec)
	r := libc_nanosleep(&ts, nil)
	if r < 0 {
		errno = GetErrno()
	}
	return
}
