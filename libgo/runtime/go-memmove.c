/* go-memmove.c -- memmove

   Copyright 2021 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

void gomemmove(void *, void *, uintptr)
  __asm__ (GOSYM_PREFIX "runtime.memmove")
  __attribute__ ((no_split_stack));

// This implementation is necessary since
// the __builtin_memmove might use __libc_memmove
// which doesn't require atomicity of 8 byte
// moves.

void
gomemmove (void *dst, void *src, uintptr len)
{
#if !defined(__PPC64__)
  __builtin_memmove(dst, src, len);
#else
  uint64 offset, tail;
  int64 rem;
  uint64 dwords;
  uint64 i;
  char *bdst,*bsrc;

  rem = len;

  if (len == 0) {
	return;
  }

  // If src and dst don't have the same 8 byte alignment then
  // there is no issue with copying pointer atomicity. Use the
  // builtin.
  if (((uint64)dst % 8) != ((uint64)src % 8) || len < 8) {
	__builtin_memmove(dst, src, len);
	return;
  }

  // Length >= 8 && same ptr alignment
  offset = (uint64)dst % 8;

  // If not 8 byte alignment, move the intial bytes.
  if (offset > 0) {
	__builtin_memmove(dst, src, 8-offset);
	dst += (8-offset);
	src += (8-offset);
	rem -= (8-offset);
  }

  // Move the tail bytes to make the backward move
  // easier.
  tail = rem % 8;
  if (tail > 0) {
	__builtin_memmove(dst+rem-tail, src+rem-tail, tail);
	rem -= tail;
  }

  if (rem == 0) {
	return;
  }

  // Must now be 8 byte alignment and rem is multiple of 8.
  dwords = len>>3;

  // Determine if a backwards move is needed
  // Forward or backward, move all doublewords

  if ((uint64)(dst - src) < (uint64)rem) {
	bdst = dst+rem-8;
	bsrc = src+rem-8;
	for (i = 0; i<dwords; i++) {
		*(uint64*)bdst = *(uint64*)bsrc;
		bdst -= 8;
		bsrc -= 8;
	}
  } else {
	for (i = 0; i<dwords; i++) {
		*(uint64*)dst = *(uint64*)src;
		dst += 8;
		src += 8;
	}
  }
#endif
}
