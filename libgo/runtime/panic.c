// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "malloc.h"
#include "go-defer.h"
#include "go-panic.h"

// Code related to defer, panic and recover.

uint32 runtime_panicking;
static Lock paniclk;

// Run all deferred functions for the current goroutine.
static void
rundefer(void)
{
	G *g;
	Defer *d;

	g = runtime_g();
	while((d = g->defer) != nil) {
		void (*pfn)(void*);

		g->defer = d->__next;
		pfn = d->__pfn;
		d->__pfn = nil;
		if (pfn != nil)
			(*pfn)(d->__arg);
		runtime_free(d);
	}
}

void
runtime_startpanic(void)
{
	M *m;

	m = runtime_m();
	if(runtime_mheap.cachealloc.size == 0) { // very early
		runtime_printf("runtime: panic before malloc heap initialized\n");
		m->mallocing = 1; // tell rest of panic not to try to malloc
	} else if(m->mcache == nil) // can happen if called from signal handler or throw
		m->mcache = runtime_allocmcache();
	if(m->dying) {
		runtime_printf("panic during panic\n");
		runtime_exit(3);
	}
	m->dying = 1;
	if(runtime_g() != nil)
		runtime_g()->writebuf = nil;
	runtime_xadd(&runtime_panicking, 1);
	runtime_lock(&paniclk);
	if(runtime_debug.schedtrace > 0 || runtime_debug.scheddetail > 0)
		runtime_schedtrace(true);
	runtime_freezetheworld();
}

void
runtime_dopanic(int32 unused __attribute__ ((unused)))
{
	G *g;
	static bool didothers;
	bool crash;
	int32 t;

	g = runtime_g();
	if(g->sig != 0)
		runtime_printf("[signal %x code=%p addr=%p]\n",
			       g->sig, (void*)g->sigcode0, (void*)g->sigcode1);

	if((t = runtime_gotraceback(&crash)) > 0){
		if(g != runtime_m()->g0) {
			runtime_printf("\n");
			runtime_goroutineheader(g);
			runtime_traceback();
			runtime_printcreatedby(g);
		} else if(t >= 2 || runtime_m()->throwing > 0) {
			runtime_printf("\nruntime stack:\n");
			runtime_traceback();
		}
		if(!didothers) {
			didothers = true;
			runtime_tracebackothers(g);
		}
	}
	runtime_unlock(&paniclk);
	if(runtime_xadd(&runtime_panicking, -1) != 0) {
		// Some other m is panicking too.
		// Let it print what it needs to print.
		// Wait forever without chewing up cpu.
		// It will exit when it's done.
		static Lock deadlock;
		runtime_lock(&deadlock);
		runtime_lock(&deadlock);
	}
	
	if(crash)
		runtime_crash();

	runtime_exit(2);
}

void
runtime_throw(const char *s)
{
	M *mp;

	mp = runtime_m();
	if(mp->throwing == 0)
		mp->throwing = 1;
	runtime_startpanic();
	runtime_printf("fatal error: %s\n", s);
	runtime_dopanic(0);
	*(int32*)0 = 0;	// not reached
	runtime_exit(1);	// even more not reached
}

void
runtime_panicstring(const char *s)
{
	Eface err;

	if(runtime_m()->mallocing) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic during malloc");
	}
	if(runtime_m()->gcing) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic during gc");
	}
	runtime_newErrorCString(s, &err);
	runtime_panic(err);
}

void runtime_Goexit (void) __asm__ (GOSYM_PREFIX "runtime.Goexit");

void
runtime_Goexit(void)
{
	rundefer();
	runtime_goexit();
}
