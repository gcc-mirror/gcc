/* go-memmove.c -- move one memory buffer to another

   Copyright 2016 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

void move(void *, void *, uintptr)
  __asm__ (GOSYM_PREFIX "runtime.memmove");

void
move (void *p1, void *p2, uintptr len)
{
  __builtin_memmove (p1, p2, len);
}
