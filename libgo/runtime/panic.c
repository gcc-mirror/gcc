// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"

extern void gothrow(String) __attribute__((noreturn));
extern void gothrow(String) __asm__(GOSYM_PREFIX "runtime.throw");

void
runtime_throw(const char *s)
{
	gothrow(runtime_gostringnocopy((const byte *)s));
}

void
runtime_panicstring(const char *s)
{
	G *gp;
	Eface err;

	gp = runtime_g();
	if (gp == nil) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic with no g");
	}
	if (gp->m == nil) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic with no m");
	}
	if (gp->m->curg != gp) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic on system stack");
	}
	if (gp->m->mallocing != 0) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic during malloc");
	}
	if (gp->m->preemptoff.len != 0) {
		runtime_printf("panic: %s\n", s);
		runtime_printf("preempt off reason: %S\n", gp->m->preemptoff);
		runtime_throw("panic during preemptoff");
	}
	if (gp->m->locks != 0) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic holding locks");
	}
	runtime_newErrorCString((uintptr) s, &err);
	runtime_panic(err);
}

extern void runtime_abort(void) __asm__(GOSYM_PREFIX "runtime.abort");

void
runtime_abort()
{
	abort();
}
