// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <complex.h>
#include <math.h>
#include <stdarg.h>
#include "runtime.h"
#include "array.h"
#include "go-type.h"

extern void runtime_printlock(void)
  __asm__(GOSYM_PREFIX "runtime.printlock");
extern void runtime_printunlock(void)
  __asm__(GOSYM_PREFIX "runtime.printunlock");
extern void gwrite(Slice)
  __asm__(GOSYM_PREFIX "runtime.gwrite");
extern void runtime_printint(int64)
  __asm__(GOSYM_PREFIX "runtime.printint");
extern void runtime_printuint(uint64)
  __asm__(GOSYM_PREFIX "runtime.printuint");
extern void runtime_printhex(uint64)
  __asm__(GOSYM_PREFIX "runtime.printhex");
extern void runtime_printfloat(float64)
  __asm__(GOSYM_PREFIX "runtime.printfloat");
extern void runtime_printcomplex(complex double)
  __asm__(GOSYM_PREFIX "runtime.printcomplex");
extern void runtime_printbool(_Bool)
  __asm__(GOSYM_PREFIX "runtime.printbool");
extern void runtime_printstring(String)
  __asm__(GOSYM_PREFIX "runtime.printstring");
extern void runtime_printpointer(void *)
  __asm__(GOSYM_PREFIX "runtime.printpointer");
extern void runtime_printslice(Slice)
  __asm__(GOSYM_PREFIX "runtime.printslice");
extern void runtime_printeface(Eface)
  __asm__(GOSYM_PREFIX "runtime.printeface");
extern void runtime_printiface(Iface)
  __asm__(GOSYM_PREFIX "runtime.printiface");

// Clang requires this function to not be inlined (see below).
static void go_vprintf(const char*, va_list)
__attribute__((noinline));

static void
runtime_prints(const char *s)
{
	Slice sl;

	// Use memcpy to avoid const-cast warning.
	memcpy(&sl.__values, &s, sizeof(char*));
	sl.__count = runtime_findnull((const byte*)s);
	sl.__capacity = sl.__count;
	gwrite(sl);
}

static void
runtime_printbyte(int8 c)
{
	Slice sl;

	sl.__values = &c;
	sl.__count = 1;
	sl.__capacity = 1;
	gwrite(sl);
}

#if defined (__clang__) && (defined (__i386__) || defined (__x86_64__))
// LLVM's code generator does not currently support split stacks for vararg
// functions, so we disable the feature for this function under Clang. This
// appears to be OK as long as:
// - this function only calls non-inlined, internal-linkage (hence no dynamic
//   loader) functions compiled with split stacks (i.e. go_vprintf), which can
//   allocate more stack space as required;
// - this function itself does not occupy more than BACKOFF bytes of stack space
//   (see libgcc/config/i386/morestack.S).
// These conditions are currently known to be satisfied by Clang on x86-32 and
// x86-64. Note that signal handlers receive slightly less stack space than they
// would normally do if they happen to be called while this function is being
// run. If this turns out to be a problem we could consider increasing BACKOFF.

void
runtime_printf(const char *s, ...)
__attribute__((no_split_stack));

int32
runtime_snprintf(byte *buf, int32 n, const char *s, ...)
__attribute__((no_split_stack));

#endif

void
runtime_printf(const char *s, ...)
{
	va_list va;

	va_start(va, s);
	go_vprintf(s, va);
	va_end(va);
}

int32
runtime_snprintf(byte *buf, int32 n, const char *s, ...)
{
	G *g = runtime_g();
	va_list va;
	int32 m;

	g->writebuf.__values = buf;
	g->writebuf.__count = 0;
	g->writebuf.__capacity = n-1;
	va_start(va, s);
	go_vprintf(s, va);
	va_end(va);
	m = g->writebuf.__count;
	((byte*)g->writebuf.__values)[m] = '\0';
	g->writebuf.__values = nil;
	g->writebuf.__count = 0;
	g->writebuf.__capacity = 0;
	return m;
}

// Very simple printf.  Only for debugging prints.
// Do not add to this without checking with Rob.
static void
go_vprintf(const char *s, va_list va)
{
	const char *p, *lp;
	Slice sl;

	runtime_printlock();

	lp = p = s;
	for(; *p; p++) {
		if(*p != '%')
			continue;
		if(p > lp) {
			// Use memcpy to avoid const-cast warning.
			memcpy(&sl.__values, &lp, sizeof(char*));
			sl.__count = p - lp;
			sl.__capacity = p - lp;
			gwrite(sl);
		}
		p++;
		switch(*p) {
		case 'a':
			runtime_printslice(va_arg(va, Slice));
			break;
		case 'c':
			runtime_printbyte(va_arg(va, int32));
			break;
		case 'd':
			runtime_printint(va_arg(va, int32));
			break;
		case 'D':
			runtime_printint(va_arg(va, int64));
			break;
		case 'e':
			runtime_printeface(va_arg(va, Eface));
			break;
		case 'f':
			runtime_printfloat(va_arg(va, float64));
			break;
		case 'C':
			runtime_printcomplex(va_arg(va, complex double));
			break;
		case 'i':
			runtime_printiface(va_arg(va, Iface));
			break;
		case 'p':
			runtime_printpointer(va_arg(va, void*));
			break;
		case 's':
			runtime_prints(va_arg(va, char*));
			break;
		case 'S':
			runtime_printstring(va_arg(va, String));
			break;
		case 't':
			runtime_printbool(va_arg(va, int));
			break;
		case 'U':
			runtime_printuint(va_arg(va, uint64));
			break;
		case 'x':
			runtime_printhex(va_arg(va, uint32));
			break;
		case 'X':
			runtime_printhex(va_arg(va, uint64));
			break;
		}
		lp = p+1;
	}
	if(p > lp) {
		// Use memcpy to avoid const-cast warning.
		memcpy(&sl.__values, &lp, sizeof(char*));
		sl.__count = p - lp;
		sl.__capacity = p - lp;
		gwrite(sl);
	}

	runtime_printunlock();
}
