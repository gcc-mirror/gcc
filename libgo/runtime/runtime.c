// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <unistd.h>

#include "runtime.h"
#include "array.h"
#include "go-panic.h"
#include "go-string.h"

uint32	runtime_panicking;

static Lock paniclk;

void
runtime_initpanic(void)
{
	runtime_initlock(&paniclk);
}

void
runtime_startpanic(void)
{
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
	/*
	static bool didothers;

	if(g->sig != 0)
		runtime_printf("[signal %x code=%p addr=%p pc=%p]\n",
			g->sig, g->sigcode0, g->sigcode1, g->sigpc);

	if(runtime_gotraceback()){
		if(!didothers) {
			didothers = true;
			runtime_tracebackothers(g);
		}
	}
	*/

	runtime_unlock(&paniclk);
	if(runtime_xadd(&runtime_panicking, -1) != 0) {
		// Some other m is panicking too.
		// Let it print what it needs to print.
		// Wait forever without chewing up cpu.
		// It will exit when it's done.
		static Lock deadlock;
		runtime_initlock(&deadlock);
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

static int32	argc;
static byte**	argv;

extern Slice os_Args asm ("libgo_os.os.Args");
extern Slice os_Envs asm ("libgo_os.os.Envs");

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
		s[i] = runtime_gostringnocopy((byte*)argv[i]);
	os_Args.__values = (void*)s;
	os_Args.__count = argc;
	os_Args.__capacity = argc;
}

void
runtime_goenvs(void)
{
	String *s;
	int32 i, n;
	
	for(n=0; argv[argc+1+n] != 0; n++)
		;

	s = runtime_malloc(n*sizeof s[0]);
	for(i=0; i<n; i++)
		s[i] = runtime_gostringnocopy(argv[argc+1+i]);
	os_Envs.__values = (void*)s;
	os_Envs.__count = n;
	os_Envs.__capacity = n;
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
	envv = (String*)os_Envs.__values;
	envc = os_Envs.__count;
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
	uint32 x;

	x = m->fastrand;
	x += x;
	if(x & 0x80000000L)
		x ^= 0x88888eefUL;
	m->fastrand = x;
	return x;
}
