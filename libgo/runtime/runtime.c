// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <unistd.h>

#include "runtime.h"
#include "array.h"
#include "go-panic.h"
#include "go-string.h"

uint32	runtime_panicking;

int32
runtime_gotraceback(void)
{
	const byte *p;

	p = runtime_getenv("GOTRACEBACK");
	if(p == nil || p[0] == '\0')
		return 1;	// default is on
	return runtime_atoi(p);
}

static Lock paniclk;

void
runtime_startpanic(void)
{
	M *m;

	m = runtime_m();
	if(m->dying) {
		runtime_printf("panic during panic\n");
		runtime_exit(3);
	}
	m->dying = 1;
	runtime_xadd(&runtime_panicking, 1);
	runtime_lock(&paniclk);
}

void
runtime_dopanic(int32 unused __attribute__ ((unused)))
{
	G* g;
	static bool didothers;

	g = runtime_g();
	if(g->sig != 0)
		runtime_printf("[signal %x code=%p addr=%p]\n",
			g->sig, (void*)(g->sigcode0), (void*)(g->sigcode1));

	if(runtime_gotraceback()){
		if(g != runtime_m()->g0) {
			runtime_printf("\n");
			runtime_goroutineheader(g);
			runtime_traceback();
			runtime_goroutinetrailer(g);
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

	runtime_exit(2);
}

void
runtime_throw(const char *s)
{
	runtime_startpanic();
	runtime_printf("throw: %s\n", s);
	runtime_dopanic(0);
	*(int32*)0 = 0;	// not reached
	runtime_exit(1);	// even more not reached
}

void
runtime_panicstring(const char *s)
{
	Eface err;

	if(runtime_m()->gcing) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic during gc");
	}
	runtime_newErrorString(runtime_gostringnocopy((const byte*)s), &err);
	runtime_panic(err);
}

static int32	argc;
static byte**	argv;

extern Slice os_Args asm ("os.Args");
extern Slice syscall_Envs asm ("syscall.Envs");

void
runtime_args(int32 c, byte **v)
{
	argc = c;
	argv = v;
}

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

const byte*
runtime_getenv(const char *s)
{
	int32 i, j, len;
	const byte *v, *bs;
	String* envv;
	int32 envc;

	bs = (const byte*)s;
	len = runtime_findnull(bs);
	envv = (String*)syscall_Envs.__values;
	envc = syscall_Envs.__count;
	for(i=0; i<envc; i++){
		if(envv[i].__length <= len)
			continue;
		v = (const byte*)envv[i].__data;
		for(j=0; j<len; j++)
			if(bs[j] != v[j])
				goto nomatch;
		if(v[len] != '=')
			goto nomatch;
		return v+len+1;
	nomatch:;
	}
	return nil;
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

static struct root_list runtime_roots =
{ nil,
  { { &syscall_Envs, sizeof syscall_Envs },
    { &os_Args, sizeof os_Args },
    { nil, 0 } },
};

void
runtime_check(void)
{
	__go_register_gc_roots(&runtime_roots);
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
runtime_showframe(const unsigned char *s)
{
	static int32 traceback = -1;
	
	if(traceback < 0)
		traceback = runtime_gotraceback();
	return traceback > 1 || (__builtin_strchr((const char*)s, '.') != nil && __builtin_memcmp(s, "runtime.", 7) != 0);
}

bool
runtime_isInf(float64 f, int32 sign)
{
	if(!__builtin_isinf(f))
		return false;
	if(sign == 0)
		return true;
	if(sign > 0)
		return f > 0;
	return f < 0;
}
