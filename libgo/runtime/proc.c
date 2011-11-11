// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "arch.h"
#include "malloc.h"	/* so that acid generated from proc.c includes malloc data structures */

typedef struct Sched Sched;

G	runtime_g0;
M	runtime_m0;

#ifdef __rtems__
#define __thread
#endif

__thread G *g;
__thread M *m;

static struct {
	Lock;
	void (*fn)(uintptr*, int32);
	int32 hz;
	uintptr pcbuf[100];
} prof;

void
runtime_sigprof(uint8 *pc __attribute__ ((unused)),
		uint8 *sp __attribute__ ((unused)),
		uint8 *lr __attribute__ ((unused)))
{
	int32 n;
	
	if(prof.fn == nil || prof.hz == 0)
		return;
	
	runtime_lock(&prof);
	if(prof.fn == nil) {
		runtime_unlock(&prof);
		return;
	}
	n = 0;
	// n = runtime·gentraceback(pc, sp, lr, gp, 0, prof.pcbuf, nelem(prof.pcbuf));
	if(n > 0)
		prof.fn(prof.pcbuf, n);
	runtime_unlock(&prof);
}

void
runtime_setcpuprofilerate(void (*fn)(uintptr*, int32), int32 hz)
{
	// Force sane arguments.
	if(hz < 0)
		hz = 0;
	if(hz == 0)
		fn = nil;
	if(fn == nil)
		hz = 0;

	// Stop profiler on this cpu so that it is safe to lock prof.
	// if a profiling signal came in while we had prof locked,
	// it would deadlock.
	runtime_resetcpuprofiler(0);

	runtime_lock(&prof);
	prof.fn = fn;
	prof.hz = hz;
	runtime_unlock(&prof);
	// runtime_lock(&runtime_sched);
	// runtime_sched.profilehz = hz;
	// runtime_unlock(&runtime_sched);
	
	if(hz != 0)
		runtime_resetcpuprofiler(hz);
}

/* The entersyscall and exitsyscall functions aren't used for anything
   yet.  Eventually they will be used to switch to a new OS thread
   when making a potentially-blocking library call.  */

void runtime_entersyscall() __asm__("libgo_syscall.syscall.entersyscall");

void
runtime_entersyscall()
{
}

void runtime_exitsyscall() __asm__("libgo_syscall.syscall.exitsyscall");

void
runtime_exitsyscall()
{
}
