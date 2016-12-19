// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"unsafe"
)

// Temporary for C code to call:
//go:linkname minit runtime.minit

// minit is called to initialize a new m (including the bootstrap m).
// Called on the new thread, cannot allocate memory.
func minit() {
	// Initialize signal handling.
	_g_ := getg()

	var st _stack_t
	sigaltstack(nil, &st)
	if st.ss_flags&_SS_DISABLE != 0 {
		signalstack(_g_.m.gsignalstack, _g_.m.gsignalstacksize)
		_g_.m.newSigstack = true
	} else {
		_g_.m.newSigstack = false
	}

	// FIXME: We should set _g_.m.procid here.

	// restore signal mask from m.sigmask and unblock essential signals
	nmask := _g_.m.sigmask
	for i := range sigtable {
		if sigtable[i].flags&_SigUnblock != 0 {
			sigdelset(&nmask, int32(i))
		}
	}
	sigprocmask(_SIG_SETMASK, &nmask, nil)
}

// Called from dropm to undo the effect of an minit.
//go:nosplit
func unminit() {
	if getg().m.newSigstack {
		signalstack(nil, 0)
	}
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
