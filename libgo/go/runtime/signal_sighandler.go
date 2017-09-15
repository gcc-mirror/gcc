// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix darwin dragonfly freebsd linux nacl netbsd openbsd solaris

package runtime

import (
	"unsafe"
)

// crashing is the number of m's we have waited for when implementing
// GOTRACEBACK=crash when a signal is received.
var crashing int32

// sighandler is invoked when a signal occurs. The global g will be
// set to a gsignal goroutine and we will be running on the alternate
// signal stack. The parameter g will be the value of the global g
// when the signal occurred. The sig, info, and ctxt parameters are
// from the system signal handler: they are the parameters passed when
// the SA is passed to the sigaction system call.
//
// The garbage collector may have stopped the world, so write barriers
// are not allowed.
//
//go:nowritebarrierrec
func sighandler(sig uint32, info *_siginfo_t, ctxt unsafe.Pointer, gp *g) {
	_g_ := getg()
	c := sigctxt{info, ctxt}

	sigfault, sigpc := getSiginfo(info, ctxt)

	if sig == _SIGPROF {
		sigprof(sigpc, gp, _g_.m)
		return
	}

	flags := int32(_SigThrow)
	if sig < uint32(len(sigtable)) {
		flags = sigtable[sig].flags
	}
	if c.sigcode() != _SI_USER && flags&_SigPanic != 0 {
		// Emulate gc by passing arguments out of band,
		// although we don't really have to.
		gp.sig = sig
		gp.sigcode0 = uintptr(c.sigcode())
		gp.sigcode1 = sigfault
		gp.sigpc = sigpc

		setg(gp)

		// All signals were blocked due to the sigaction mask;
		// unblock them.
		var set sigset
		sigfillset(&set)
		sigprocmask(_SIG_UNBLOCK, &set, nil)

		sigpanic()
		throw("sigpanic returned")
	}

	if c.sigcode() == _SI_USER || flags&_SigNotify != 0 {
		if sigsend(sig) {
			return
		}
	}

	if c.sigcode() == _SI_USER && signal_ignored(sig) {
		return
	}

	if flags&_SigKill != 0 {
		dieFromSignal(sig)
	}

	if flags&_SigThrow == 0 {
		return
	}

	_g_.m.throwing = 1
	_g_.m.caughtsig.set(gp)

	if crashing == 0 {
		startpanic()
	}

	if sig < uint32(len(sigtable)) {
		print(sigtable[sig].name, "\n")
	} else {
		print("Signal ", sig, "\n")
	}

	print("PC=", hex(sigpc), " m=", _g_.m.id, " sigcode=", c.sigcode(), "\n")
	if _g_.m.lockedg != nil && _g_.m.ncgo > 0 && gp == _g_.m.g0 {
		print("signal arrived during cgo execution\n")
		gp = _g_.m.lockedg
	}
	print("\n")

	level, _, docrash := gotraceback()
	if level > 0 {
		goroutineheader(gp)
		traceback(0)
		if crashing == 0 {
			tracebackothers(gp)
			print("\n")
		}
		dumpregs(info, ctxt)
	}

	if docrash {
		crashing++
		if crashing < sched.mcount-int32(extraMCount) {
			// There are other m's that need to dump their stacks.
			// Relay SIGQUIT to the next m by sending it to the current process.
			// All m's that have already received SIGQUIT have signal masks blocking
			// receipt of any signals, so the SIGQUIT will go to an m that hasn't seen it yet.
			// When the last m receives the SIGQUIT, it will fall through to the call to
			// crash below. Just in case the relaying gets botched, each m involved in
			// the relay sleeps for 5 seconds and then does the crash/exit itself.
			// In expected operation, the last m has received the SIGQUIT and run
			// crash/exit and the process is gone, all long before any of the
			// 5-second sleeps have finished.
			print("\n-----\n\n")
			raiseproc(_SIGQUIT)
			usleep(5 * 1000 * 1000)
		}
		crash()
	}

	exit(2)
}
