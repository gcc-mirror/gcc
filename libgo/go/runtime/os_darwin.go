// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

type mOS struct {
	initialized bool
	mutex       pthreadmutex
	cond        pthreadcond
	count       int
}

func unimplemented(name string) {
	println(name, "not implemented")
	*(*int)(unsafe.Pointer(uintptr(1231))) = 1231
}

//go:nosplit
func semacreate(mp *m) {
	if mp.initialized {
		return
	}
	mp.initialized = true
	if err := pthread_mutex_init(&mp.mutex, nil); err != 0 {
		throw("pthread_mutex_init")
	}
	if err := pthread_cond_init(&mp.cond, nil); err != 0 {
		throw("pthread_cond_init")
	}
}

//go:nosplit
func semasleep(ns int64) int32 {
	mp := getg().m
	pthread_mutex_lock(&mp.mutex)
	for {
		if mp.count > 0 {
			mp.count--
			pthread_mutex_unlock(&mp.mutex)
			return 0
		}
		if ns >= 0 {
			var t timespec
			t.set_nsec(ns)
			err := pthread_cond_timedwait_relative_np(&mp.cond, &mp.mutex, &t)
			if err == _ETIMEDOUT {
				pthread_mutex_unlock(&mp.mutex)
				return -1
			}
		} else {
			pthread_cond_wait(&mp.cond, &mp.mutex)
		}
	}
}

//go:nosplit
func semawakeup(mp *m) {
	pthread_mutex_lock(&mp.mutex)
	mp.count++
	if mp.count > 0 {
		pthread_cond_signal(&mp.cond)
	}
	pthread_mutex_unlock(&mp.mutex)
}
