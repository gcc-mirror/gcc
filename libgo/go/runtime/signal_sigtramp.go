// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin dragonfly freebsd linux netbsd openbsd solaris

package runtime

import "unsafe"

// For gccgo, use go:linkname so the C signal handler can call this one.
//go:linkname sigtrampgo runtime.sigtrampgo

// Continuation of the (assembly) sigtramp() logic.
// This may be called with the world stopped.
//go:nosplit
//go:nowritebarrierrec
func sigtrampgo(sig uint32, info *_siginfo_t, ctx unsafe.Pointer) {
	if sigfwdgo(sig, info, ctx) {
		return
	}
	g := getg()
	if g == nil {
		if sig == _SIGPROF {
			// Ignore profiling signals that arrive on
			// non-Go threads. On some systems they will
			// be handled directly by the signal handler,
			// by calling sigprofNonGo, in which case we won't
			// get here anyhow.
			return
		}
		badsignal(uintptr(sig), &sigctxt{info, ctx})
		return
	}

	setg(g.m.gsignal)
	sighandler(sig, info, ctx, g)
	setg(g)
}
