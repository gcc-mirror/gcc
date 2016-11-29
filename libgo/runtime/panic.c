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
	Eface err;

	if(runtime_m()->mallocing) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic during malloc");
	}
	if(runtime_m()->gcing) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic during gc");
	}
	if(runtime_m()->locks) {
		runtime_printf("panic: %s\n", s);
		runtime_throw("panic holding locks");
	}
	runtime_newErrorCString(s, &err);
	runtime_panic(err);
}
