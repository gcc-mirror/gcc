// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build aix darwin dragonfly freebsd hurd linux netbsd openbsd solaris

package runtime

import (
	"unsafe"
)

// Functions for gccgo to support signal handling. In the gc runtime
// these are written in OS-specific files and in assembler.

//go:noescape
//extern sigaction
func sigaction(signum uint32, act *_sigaction, oact *_sigaction) int32

//go:noescape
//extern sigprocmask
func sigprocmask(how int32, set *sigset, oldset *sigset) int32

//go:noescape
//extern sigfillset
func sigfillset(set *sigset) int32

//go:noescape
//extern sigemptyset
func sigemptyset(set *sigset) int32

//go:noescape
//extern sigaddset
func c_sigaddset(set *sigset, signum uint32) int32

//go:noescape
//extern sigdelset
func c_sigdelset(set *sigset, signum uint32) int32

//go:noescape
//extern sigaltstack
func sigaltstack(ss *_stack_t, oss *_stack_t) int32

//extern raise
func raise(sig uint32) int32

//extern getpid
func getpid() _pid_t

//extern kill
func kill(pid _pid_t, sig uint32) int32

//go:noescape
//extern setitimer
func setitimer(which int32, new *_itimerval, old *_itimerval) int32

type sigctxt struct {
	info *_siginfo_t
	ctxt unsafe.Pointer
}

func (c *sigctxt) sigcode() uint64 {
	if c.info == nil {
		// This can happen on Solaris 10.  We don't know the
		// code, just avoid a misleading value.
		return _SI_USER + 1
	}
	return uint64(c.info.si_code)
}

//go:nosplit
//go:nowritebarrierrec
func setsig(i uint32, fn uintptr) {
	var sa _sigaction
	sa.sa_flags = _SA_SIGINFO | _SA_RESTART

	// For gccgo we do not set SA_ONSTACK for a signal that can
	// cause a panic.  Instead, we trust that the split stack has
	// enough room to start the signal handler.  This is because
	// otherwise we have no good way to switch back to the
	// original stack before panicing.
	if sigtable[i].flags&_SigPanic == 0 {
		sa.sa_flags |= _SA_ONSTACK
	}

	sigfillset((*sigset)(unsafe.Pointer(&sa.sa_mask)))
	setSigactionHandler(&sa, fn)
	sigaction(i, &sa, nil)
}

//go:nosplit
//go:nowritebarrierrec
func setsigstack(i uint32) {
	var sa _sigaction
	sigaction(i, nil, &sa)
	handler := getSigactionHandler(&sa)
	if handler == 0 || handler == _SIG_DFL || handler == _SIG_IGN || sa.sa_flags&_SA_ONSTACK != 0 {
		return
	}
	if sigtable[i].flags&_SigPanic != 0 {
		return
	}
	sa.sa_flags |= _SA_ONSTACK
	sigaction(i, &sa, nil)
}

//go:nosplit
//go:nowritebarrierrec
func getsig(i uint32) uintptr {
	var sa _sigaction
	if sigaction(i, nil, &sa) < 0 {
		// On GNU/Linux glibc rejects attempts to call
		// sigaction with signal 32 (SIGCANCEL) or 33 (SIGSETXID).
		if GOOS == "linux" && (i == 32 || i == 33) {
			return _SIG_DFL
		}
		throw("sigaction read failure")
	}
	return getSigactionHandler(&sa)
}

func signalstack(p unsafe.Pointer, n uintptr)

//go:nosplit
//go:nowritebarrierrec
func raiseproc(sig uint32) {
	kill(getpid(), sig)
}

//go:nosplit
//go:nowritebarrierrec
func sigfwd(fn uintptr, sig uint32, info *_siginfo_t, ctx unsafe.Pointer) {
	f1 := [1]uintptr{fn}
	f2 := &f1
	f3 := *(*func(uint32, *_siginfo_t, unsafe.Pointer))(unsafe.Pointer(&f2))
	f3(sig, info, ctx)
}

//go:nosplit
//go:nowritebarrierrec
func sigaddset(mask *sigset, i int) {
	c_sigaddset(mask, uint32(i))
}

func sigdelset(mask *sigset, i int) {
	c_sigdelset(mask, uint32(i))
}
