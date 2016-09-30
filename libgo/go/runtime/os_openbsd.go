// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/atomic"
	"unsafe"
)

type mOS struct {
	waitsemacount uint32
}

//go:noescape
//extern thrsleep
func thrsleep(ident uintptr, clock_id int32, tsp *timespec, lock uintptr, abort *uint32) int32

//go:noescape
//extern thrwakeup
func thrwakeup(ident uintptr, n int32) int32

//go:nosplit
func semacreate(mp *m) {
}

//go:nosplit
func semasleep(ns int64) int32 {
	_g_ := getg()

	// Compute sleep deadline.
	var tsp *timespec
	if ns >= 0 {
		var ts timespec
		var nsec int32
		ns += nanotime()
		ts.set_sec(int64(timediv(ns, 1000000000, &nsec)))
		ts.set_nsec(nsec)
		tsp = &ts
	}

	for {
		v := atomic.Load(&_g_.m.mos.waitsemacount)
		if v > 0 {
			if atomic.Cas(&_g_.m.mos.waitsemacount, v, v-1) {
				return 0 // semaphore acquired
			}
			continue
		}

		// Sleep until woken by semawakeup or timeout; or abort if waitsemacount != 0.
		//
		// From OpenBSD's __thrsleep(2) manual:
		// "The abort argument, if not NULL, points to an int that will
		// be examined [...] immediately before blocking. If that int
		// is non-zero then __thrsleep() will immediately return EINTR
		// without blocking."
		ret := thrsleep(uintptr(unsafe.Pointer(&_g_.m.mos.waitsemacount)), _CLOCK_MONOTONIC, tsp, 0, &_g_.m.mos.waitsemacount)
		if ret == _EWOULDBLOCK {
			return -1
		}
	}
}

//go:nosplit
func semawakeup(mp *m) {
	atomic.Xadd(&mp.mos.waitsemacount, 1)
	ret := thrwakeup(uintptr(unsafe.Pointer(&mp.mos.waitsemacount)), 1)
	if ret != 0 && ret != _ESRCH {
		// semawakeup can be called on signal stack.
		systemstack(func() {
			print("thrwakeup addr=", &mp.mos.waitsemacount, " sem=", mp.mos.waitsemacount, " ret=", ret, "\n")
		})
	}
}
