// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build linux

package runtime

func sbrk0() uintptr

func gettid() _pid_t {
	return _pid_t(syscall(_SYS_gettid, 0, 0, 0, 0, 0, 0))
}

func tgkill(pid _pid_t, tid _pid_t, sig uint32) uint32 {
	return uint32(syscall(_SYS_tgkill, uintptr(pid), uintptr(tid), uintptr(sig), 0, 0, 0))
}
