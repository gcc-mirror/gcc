// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"unsafe"
)

// For C code to call:
//go:linkname minit

func goenvs() {
	goenvs_unix()
}

// Called to initialize a new m (including the bootstrap m).
// Called on the parent thread (main thread in case of bootstrap), can allocate memory.
func mpreinit(mp *m) {
	mp.gsignal = malg(true, true, &mp.gsignalstack, &mp.gsignalstacksize)
	mp.gsignal.m = mp
}

// minit is called to initialize a new m (including the bootstrap m).
// Called on the new thread, cannot allocate memory.
func minit() {
	minitSignals()

	// FIXME: only works on linux for now.
	getg().m.procid = uint64(gettid())
}

// Called from dropm to undo the effect of an minit.
//go:nosplit
//go:nowritebarrierrec
func unminit() {
	unminitSignals()
}

var urandom_dev = []byte("/dev/urandom\x00")

func getRandomData(r []byte) {
	if startupRandomData != nil {
		n := copy(r, startupRandomData)
		extendRandom(r, n)
		return
	}
	fd := open(&urandom_dev[0], 0 /* O_RDONLY */, 0)
	n := read(fd, unsafe.Pointer(&r[0]), int32(len(r)))
	closefd(fd)
	extendRandom(r, int(n))
}

//go:noescape
//extern pipe
func libcPipe(*[2]int32) int32

func pipe() (r, w int32, e int32) {
	var p [2]int32
	res := libcPipe(&p)
	if res < 0 {
		e = int32(errno())
	}
	return p[0], p[1], e
}

//go:noescape
//extern pipe2
func libcPipe2(*[2]int32, int32) int32

func pipe2(flags int32) (r, w int32, e int32) {
	var p [2]int32
	res := libcPipe2(&p, flags)
	if res < 0 {
		e = int32(errno())
	}
	return p[0], p[1], e
}

//extern __go_fcntl_uintptr
func fcntlUintptr(fd, cmd, arg uintptr) (uintptr, uintptr)

//go:nosplit
func closeonexec(fd int32) {
	fcntlUintptr(uintptr(fd), _F_SETFD, _FD_CLOEXEC)
}

//go:nosplit
func setNonblock(fd int32) {
	flags, errno := fcntlUintptr(uintptr(fd), _F_GETFL, 0)
	if errno == 0 {
		fcntlUintptr(uintptr(fd), _F_SETFL, flags|_O_NONBLOCK)
	}
}
