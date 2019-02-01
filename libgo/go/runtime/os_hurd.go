// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file is derived from os_solaris.go.

package runtime

import "unsafe"

type mOS struct {
	waitsema uintptr // semaphore for parking on locks
}

//extern malloc
func libc_malloc(uintptr) unsafe.Pointer

//go:noescape
//extern sem_init
func sem_init(sem *_sem_t, pshared int32, value uint32) int32

//go:noescape
//extern sem_wait
func sem_wait(sem *_sem_t) int32

//go:noescape
//extern sem_post
func sem_post(sem *_sem_t) int32

//go:noescape
//extern sem_timedwait
func sem_timedwait(sem *_sem_t, timeout *timespec) int32

//go:nosplit
func semacreate(mp *m) {
	if mp.mos.waitsema != 0 {
		return
	}

	var sem *_sem_t

	// Call libc's malloc rather than malloc. This will
	// allocate space on the C heap. We can't call malloc
	// here because it could cause a deadlock.
	sem = (*_sem_t)(libc_malloc(unsafe.Sizeof(*sem)))
	if sem_init(sem, 0, 0) != 0 {
		throw("sem_init")
	}
	mp.mos.waitsema = uintptr(unsafe.Pointer(sem))
}

//go:nosplit
func semasleep(ns int64) int32 {
	_m_ := getg().m
	if ns >= 0 {
		var ts timespec
		ts.set_sec(ns / 1000000000)
		ts.set_nsec(int32(ns % 1000000000))

		if sem_timedwait((*_sem_t)(unsafe.Pointer(_m_.mos.waitsema)), &ts) != 0 {
			err := errno()
			if err == _ETIMEDOUT || err == _EAGAIN || err == _EINTR {
				return -1
			}
			throw("sem_timedwait")
		}
		return 0
	}
	for {
		r1 := sem_wait((*_sem_t)(unsafe.Pointer(_m_.mos.waitsema)))
		if r1 == 0 {
			break
		}
		if errno() == _EINTR {
			continue
		}
		throw("sem_wait")
	}
	return 0
}

//go:nosplit
func semawakeup(mp *m) {
	if sem_post((*_sem_t)(unsafe.Pointer(mp.mos.waitsema))) != 0 {
		throw("sem_post")
	}
}
