/* go-memclr.c -- clear a memory buffer

   Copyright 2016 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

void memclr(void *, uintptr)
  __asm__ (GOSYM_PREFIX "runtime.memclr");

void
memclr (void *p1, uintptr len)
{
  __builtin_memset (p1, 0, len);
}
