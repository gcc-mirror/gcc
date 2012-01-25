// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"

#include <errno.h>
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
	runtime_printf("futexwakeup addr=%p returned %lld\n", addr, (long long)ret);
	*(int32*)0x1006 = 0x1006;
}

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif

static int32
getproccount(void)
{
	int32 fd, rd, cnt, cpustrlen;
	const char *cpustr;
	const byte *pos;
	byte *bufpos;
	byte buf[256];

	fd = open("/proc/stat", O_RDONLY|O_CLOEXEC, 0);
	if(fd == -1)
		return 1;
	cnt = 0;
	bufpos = buf;
	cpustr = "\ncpu";
	cpustrlen = strlen(cpustr);
	for(;;) {
		rd = read(fd, bufpos, sizeof(buf)-cpustrlen);
		if(rd == -1)
			break;
		bufpos[rd] = 0;
		for(pos=buf; (pos=(const byte*)strstr((const char*)pos, cpustr)) != nil; cnt++, pos++) {
		}
		if(rd < cpustrlen)
			break;
		memmove(buf, bufpos+rd-cpustrlen+1, cpustrlen-1);
		bufpos = buf+cpustrlen-1;
	}
	close(fd);
	return cnt ? cnt : 1;
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
