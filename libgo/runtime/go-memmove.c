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
// which doesn't require atomicity of pointer-sized
// moves.

void
gomemmove(void *dst, void *src, uintptr len)
{
  const uintptr ptr_size = sizeof(dst);
  uintptr tail;
  uintptr rem;
  uintptr dwords;
  uintptr i;
  char *bdst, *bsrc;

  if (len == 0) {
    return;
  }

  // We expect pointer-containing values to be pointer-aligned.
  // If these pointers are not aligned, they don't contain pointers.
  if ((uintptr)dst % ptr_size != 0 || (uintptr)src % ptr_size != 0 || len < ptr_size) {
    __builtin_memmove(dst, src, len);
    return;
  }

  bdst = (char*)dst;
  bsrc = (char*)src;

  // Move the tail bytes to make the backward move easier.
  rem = len;
  tail = rem % ptr_size;
  if (tail > 0) {
    __builtin_memmove(bdst+rem-tail, bsrc+rem-tail, tail);
    rem -= tail;
  }

  // Must now be pointer alignment and rem is multiple of ptr_size.
  dwords = rem / ptr_size;

  // Determine if a backwards move is needed.
  // Forward or backward, move all words.

  if ((uintptr)(bdst - bsrc) < rem) {
    bdst += rem - ptr_size;
    bsrc += rem - ptr_size;
    for (i = 0; i<dwords; i++) {
      *(uintptr*)bdst = *(uintptr*)bsrc;
      bdst -= ptr_size;
      bsrc -= ptr_size;
    }
  } else {
    for (i = 0; i<dwords; i++) {
      *(uintptr*)bdst = *(uintptr*)bsrc;
      bdst += ptr_size;
      bsrc += ptr_size;
    }
  }
}
