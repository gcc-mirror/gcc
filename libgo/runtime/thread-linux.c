// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "defs.h"

// Linux futex.
//
//	futexsleep(uint32 *addr, uint32 val)
//	futexwakeup(uint32 *addr)
//
// Futexsleep atomically checks if *addr == val and if so, sleeps on addr.
// Futexwakeup wakes up threads sleeping on addr.
// Futexsleep is allowed to wake up spuriously.

#include <errno.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <syscall.h>
#include <linux/futex.h>

typedef struct timespec Timespec;

// Atomically,
//	if(*addr == val) sleep
// Might be woken up spuriously; that's allowed.
// Don't sleep longer than ns; ns < 0 means forever.
void
runtime_futexsleep(uint32 *addr, uint32 val, int64 ns)
{
	Timespec ts, *tsp;

	if(ns < 0)
		tsp = nil;
	else {
		ts.tv_sec = ns/1000000000LL;
		ts.tv_nsec = ns%1000000000LL;
		// Avoid overflow
		if(ts.tv_sec > 1<<30)
			ts.tv_sec = 1<<30;
		tsp = &ts;
	}

	// Some Linux kernels have a bug where futex of
	// FUTEX_WAIT returns an internal error code
	// as an errno.  Libpthread ignores the return value
	// here, and so can we: as it says a few lines up,
	// spurious wakeups are allowed.
	syscall(__NR_futex, addr, FUTEX_WAIT, val, tsp, nil, 0);
}

// If any procs are sleeping on addr, wake up at most cnt.
void
runtime_futexwakeup(uint32 *addr, uint32 cnt)
{
	int64 ret;

	ret = syscall(__NR_futex, addr, FUTEX_WAKE, cnt, nil, nil, 0);

	if(ret >= 0)
		return;

	// I don't know that futex wakeup can return
	// EAGAIN or EINTR, but if it does, it would be
	// safe to loop and call futex again.
	runtime_printf("futexwakeup addr=%p returned %D\n", addr, ret);
	*(int32*)0x1006 = 0x1006;
}

void
runtime_osinit(void)
{
	runtime_ncpu = getproccount();
}

void
runtime_goenvs(void)
{
	runtime_goenvs_unix();
}

// Called to initialize a new m (including the bootstrap m).
// Called on the parent thread (main thread in case of bootstrap), can allocate memory.
void
runtime_mpreinit(M *mp)
{
	mp->gsignal = runtime_malg(32*1024, &mp->gsignalstack, &mp->gsignalstacksize);	// OS X wants >=8K, Linux >=2K
}

// Called to initialize a new m (including the bootstrap m).
// Called on the new thread, can not allocate memory.
void
runtime_minit(void)
{
	M* m;
	sigset_t sigs;

	// Initialize signal handling.
	m = runtime_m();
	runtime_signalstack(m->gsignalstack, m->gsignalstacksize);
	if (sigemptyset(&sigs) != 0)
		runtime_throw("sigemptyset");
	sigprocmask(SIG_SETMASK, &sigs, nil);
}

// Called from dropm to undo the effect of an minit.
void
runtime_unminit(void)
{
	runtime_signalstack(nil, 0);
}

void
runtime_signalstack(byte *p, int32 n)
{
	stack_t st;

	st.ss_sp = p;
	st.ss_size = n;
	st.ss_flags = 0;
	if(p == nil)
		st.ss_flags = SS_DISABLE;
	if(sigaltstack(&st, nil) < 0)
		*(int *)0xf1 = 0xf1;
}
