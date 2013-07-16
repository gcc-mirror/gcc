// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <unistd.h>

#include "config.h"

#include "runtime.h"
#include "array.h"
#include "go-panic.h"

// The GOTRACEBACK environment variable controls the
// behavior of a Go program that is crashing and exiting.
//	GOTRACEBACK=0   suppress all tracebacks
//	GOTRACEBACK=1   default behavior - show tracebacks but exclude runtime frames
//	GOTRACEBACK=2   show tracebacks including runtime frames
//	GOTRACEBACK=crash   show tracebacks including runtime frames, then crash (core dump etc)
int32
runtime_gotraceback(bool *crash)
{
	const byte *p;

	if(crash != nil)
		*crash = false;
	p = runtime_getenv("GOTRACEBACK");
	if(p == nil || p[0] == '\0')
		return 1;	// default is on
	if(runtime_strcmp((const char *)p, "crash") == 0) {
		if(crash != nil)
			*crash = true;
		return 2;	// extra information
	}
	return runtime_atoi(p);
}

static int32	argc;
static byte**	argv;

extern Slice os_Args __asm__ (GOSYM_PREFIX "os.Args");
extern Slice syscall_Envs __asm__ (GOSYM_PREFIX "syscall.Envs");

void (*runtime_sysargs)(int32, uint8**);

void
runtime_args(int32 c, byte **v)
{
	argc = c;
	argv = v;
	if(runtime_sysargs != nil)
		runtime_sysargs(c, v);
}

byte*
runtime_progname()
{
  return argc == 0 ? nil : argv[0];
}

// Information about what cpu features are available.
// Set on startup in asm_{x86/amd64}.s.
uint32 runtime_cpuid_ecx;
uint32 runtime_cpuid_edx;

void
runtime_goargs(void)
{
	String *s;
	int32 i;

	// for windows implementation see "os" package
	if(Windows)
		return;

	s = runtime_malloc(argc*sizeof s[0]);
	for(i=0; i<argc; i++)
		s[i] = runtime_gostringnocopy((const byte*)argv[i]);
	os_Args.__values = (void*)s;
	os_Args.__count = argc;
	os_Args.__capacity = argc;
}

void
runtime_goenvs_unix(void)
{
	String *s;
	int32 i, n;

	for(n=0; argv[argc+1+n] != 0; n++)
		;

	s = runtime_malloc(n*sizeof s[0]);
	for(i=0; i<n; i++)
		s[i] = runtime_gostringnocopy(argv[argc+1+i]);
	syscall_Envs.__values = (void*)s;
	syscall_Envs.__count = n;
	syscall_Envs.__capacity = n;
}

int32
runtime_atoi(const byte *p)
{
	int32 n;

	n = 0;
	while('0' <= *p && *p <= '9')
		n = n*10 + *p++ - '0';
	return n;
}

static struct root_list runtime_roots =
{ nil,
  { { &syscall_Envs, sizeof syscall_Envs },
    { &os_Args, sizeof os_Args },
    { nil, 0 } },
};

static void
TestAtomic64(void)
{
	uint64 z64, x64;

	z64 = 42;
	x64 = 0;
	PREFETCH(&z64);
	if(runtime_cas64(&z64, &x64, 1))
		runtime_throw("cas64 failed");
	if(x64 != 42)
		runtime_throw("cas64 failed");
	if(!runtime_cas64(&z64, &x64, 1))
		runtime_throw("cas64 failed");
	if(x64 != 42 || z64 != 1)
		runtime_throw("cas64 failed");
	if(runtime_atomicload64(&z64) != 1)
		runtime_throw("load64 failed");
	runtime_atomicstore64(&z64, (1ull<<40)+1);
	if(runtime_atomicload64(&z64) != (1ull<<40)+1)
		runtime_throw("store64 failed");
	if(runtime_xadd64(&z64, (1ull<<40)+1) != (2ull<<40)+2)
		runtime_throw("xadd64 failed");
	if(runtime_atomicload64(&z64) != (2ull<<40)+2)
		runtime_throw("xadd64 failed");
	if(runtime_xchg64(&z64, (3ull<<40)+3) != (2ull<<40)+2)
		runtime_throw("xchg64 failed");
	if(runtime_atomicload64(&z64) != (3ull<<40)+3)
		runtime_throw("xchg64 failed");
}

void
runtime_check(void)
{
	__go_register_gc_roots(&runtime_roots);

	TestAtomic64();
}

uint32
runtime_fastrand1(void)
{
	M *m;
	uint32 x;

	m = runtime_m();
	x = m->fastrand;
	x += x;
	if(x & 0x80000000L)
		x ^= 0x88888eefUL;
	m->fastrand = x;
	return x;
}

int64
runtime_cputicks(void)
{
#if defined(__386__) || defined(__x86_64__)
  uint32 low, high;
  asm("rdtsc" : "=a" (low), "=d" (high));
  return (int64)(((uint64)high << 32) | (uint64)low);
#else
  // FIXME: implement for other processors.
  return 0;
#endif
}

bool
runtime_showframe(String s, bool current)
{
	static int32 traceback = -1;

	if(current && runtime_m()->throwing > 0)
		return 1;
	if(traceback < 0)
		traceback = runtime_gotraceback(nil);
	return traceback > 1 || (__builtin_memchr(s.str, '.', s.len) != nil && __builtin_memcmp(s.str, "runtime.", 7) != 0);
}

static Lock ticksLock;
static int64 ticks;

int64
runtime_tickspersecond(void)
{
	int64 res, t0, t1, c0, c1;

	res = (int64)runtime_atomicload64((uint64*)&ticks);
	if(res != 0)
		return ticks;
	runtime_lock(&ticksLock);
	res = ticks;
	if(res == 0) {
		t0 = runtime_nanotime();
		c0 = runtime_cputicks();
		runtime_usleep(100*1000);
		t1 = runtime_nanotime();
		c1 = runtime_cputicks();
		if(t1 == t0)
			t1++;
		res = (c1-c0)*1000*1000*1000/(t1-t0);
		if(res == 0)
			res++;
		runtime_atomicstore64((uint64*)&ticks, res);
	}
	runtime_unlock(&ticksLock);
	return res;
}

int64 runtime_pprof_runtime_cyclesPerSecond(void)
     __asm__ (GOSYM_PREFIX "runtime_pprof.runtime_cyclesPerSecond");

int64
runtime_pprof_runtime_cyclesPerSecond(void)
{
	return runtime_tickspersecond();
}
