/* bytealg.c -- gccgo implementations of bytealg functions

   Copyright 2018 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>
#include <string.h>

#include "runtime.h"
#include "array.h"

#ifndef HAVE_MEMMEM

#define memmem goMemmem

static const void *goMemmem(const void *in, size_t inl, const void *s, size_t sl) {
	const char *p;
	char first;
	const char *stop;

	if (sl == 0) {
		return in;
	}
	if (inl < sl) {
		return nil;
	}
	first = *(const char *)(s);
	stop = (const char *)(in) + (inl - sl);
	for (p = (const char *)(in); p <= stop; p++) {
		if (*p == first && __builtin_memcmp(p + 1, (const char *)(s) + 1, sl - 1) == 0) {
			return (const void *)(p);
		}
	}
	return nil;
}

#endif

intgo Compare(struct __go_open_array, struct __go_open_array)
  __asm__(GOSYM_PREFIX "internal..z2fbytealg.Compare")
  __attribute__((no_split_stack));

intgo Compare(struct __go_open_array a, struct __go_open_array b)
{
	intgo l;

	l = a.__count;
	if (b.__count < l) {
		l = b.__count;
	}
	if (l > 0 && a.__values != b.__values) {
		int i;

		i = __builtin_memcmp(a.__values, b.__values, (size_t)(l));
		if (i < 0) {
			return -1;
		} else if (i > 0) {
			return 1;
		}
	}
	if (a.__count < b.__count) {
		return -1;
	}
	if (a.__count > b.__count) {
		return +1;
	}
	return 0;
}

intgo IndexByte(struct __go_open_array, byte)
  __asm__(GOSYM_PREFIX "internal..z2fbytealg.IndexByte")
  __attribute__((no_split_stack));

intgo IndexByte(struct __go_open_array b, byte c)
{
	const byte *p;

	p = __builtin_memchr(b.__values, c, (size_t)(b.__count));
	if (p == nil) {
		return -1;
	}
	return p - (byte *)(b.__values);
}


intgo IndexByteString(String, byte)
  __asm__(GOSYM_PREFIX "internal..z2fbytealg.IndexByteString")
  __attribute__((no_split_stack));

intgo IndexByteString(String s, byte c)
{
	const byte *p;

	p = __builtin_memchr(s.str, c, (size_t)(s.len));
	if (p == nil) {
		return -1;
	}
	return p - s.str;
}

intgo Index(struct __go_open_array, struct __go_open_array)
  __asm__(GOSYM_PREFIX "internal..z2fbytealg.Index")
  __attribute__((no_split_stack));

intgo Index(struct __go_open_array a, struct __go_open_array b)
{
	const byte* p;

	p = memmem(a.__values, a.__count, b.__values, b.__count);
	if (p == nil) {
		return -1;
	}
	return p - (const byte*)(a.__values);
}

intgo IndexString(String, String)
  __asm__(GOSYM_PREFIX "internal..z2fbytealg.IndexString")
  __attribute__((no_split_stack));

intgo IndexString(String a, String b)
{
	const byte* p;

	p = memmem(a.str, a.len, b.str, b.len);
	if (p == nil) {
		return -1;
	}
	return p - a.str;
}
