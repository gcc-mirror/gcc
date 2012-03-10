// Copyright 2012 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin freebsd linux openbsd netbsd

#include <sys/time.h>

#include "runtime.h"
#include "defs.h"

extern SigTab runtime_sigtab[];

void
runtime_initsig(void)
{
	int32 i;
	SigTab *t;

	// First call: basic setup.
	for(i = 0; runtime_sigtab[i].sig != -1; i++) {
		t = &runtime_sigtab[i];
		if((t->flags == 0) || (t->flags & SigDefault))
			continue;
		runtime_setsig(i, false, true);
	}
}

void
runtime_sigenable(uint32 sig)
{
	int32 i;
	SigTab *t;

	for(i = 0; runtime_sigtab[i].sig != -1; i++) {
		// ~0 means all signals.
		if(~sig == 0 || runtime_sigtab[i].sig == (int32)sig) {
			t = &runtime_sigtab[i];
			if(t->flags & SigDefault) {
				runtime_setsig(i, false, true);
				t->flags &= ~SigDefault;  // make this idempotent
			}
		}
	}
}

void
runtime_resetcpuprofiler(int32 hz)
{
	struct itimerval it;

	runtime_memclr((byte*)&it, sizeof it);
	if(hz == 0) {
		runtime_setitimer(ITIMER_PROF, &it, nil);
		runtime_setprof(false);
	} else {
		it.it_interval.tv_sec = 0;
		it.it_interval.tv_usec = 1000000 / hz;
		it.it_value = it.it_interval;
		runtime_setitimer(ITIMER_PROF, &it, nil);
		runtime_setprof(true);
	}
	runtime_m()->profilehz = hz;
}
