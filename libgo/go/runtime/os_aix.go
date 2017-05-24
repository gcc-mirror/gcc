// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

type mOS struct {
	waitsema uintptr // semaphore for parking on locks
}

//extern malloc
func libc_malloc(uintptr) unsafe.Pointer

//go:noescape
//extern sem_init
func sem_init(sem *semt, pshared int32, value uint32) int32

//go:noescape
//extern sem_wait
func sem_wait(sem *semt) int32

//go:noescape
//extern sem_post
func sem_post(sem *semt) int32

//go:noescape
//extern sem_timedwait
func sem_timedwait(sem *semt, timeout *timespec) int32

//go:noescape
//extern clock_gettime
func clock_gettime(clock_id int64, timeout *timespec) int32

//go:nosplit
func semacreate(mp *m) {
	if mp.mos.waitsema != 0 {
		return
	}

	var sem *semt

	// Call libc's malloc rather than malloc. This will
	// allocate space on the C heap. We can't call malloc
	// here because it could cause a deadlock.
	sem = (*semt)(libc_malloc(unsafe.Sizeof(*sem)))
	if sem_init(sem, 0, 0) != 0 {
		throw("sem_init")
	}
	mp.mos.waitsema = uintptr(unsafe.Pointer(sem))
}

//go:nosplit
func semasleep(ns int64) int32 {
	_m_ := getg().m
	if ns >= 0 {
		const CLOCK_REALTIME int64 = 9
		var ts timespec

		if clock_gettime(CLOCK_REALTIME, &ts) != 0 {
			throw("clock_gettime")
		}
		ts.tv_sec += timespec_sec_t(ns / 1000000000)
		ts.tv_nsec += timespec_nsec_t(ns % 1000000000)
		if ts.tv_nsec >= 1000000000 {
			ts.tv_sec += timespec_sec_t(1)
			ts.tv_nsec -= timespec_nsec_t(1000000000)
		}

		if sem_timedwait((*semt)(unsafe.Pointer(_m_.mos.waitsema)), &ts) != 0 {
			err := errno()
			if err == _ETIMEDOUT || err == _EAGAIN || err == _EINTR {
				return -1
			}
			throw("sem_timedwait")
		}
		return 0
	}
	for {
		r1 := sem_wait((*semt)(unsafe.Pointer(_m_.mos.waitsema)))
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
	if sem_post((*semt)(unsafe.Pointer(mp.mos.waitsema))) != 0 {
		throw("sem_post")
	}
}
