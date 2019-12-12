/* go-memclr.c -- clear a memory buffer

   Copyright 2016 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

void memclrNoHeapPointers(void *, uintptr)
  __asm__ (GOSYM_PREFIX "runtime.memclrNoHeapPointers")
  __attribute__ ((no_split_stack));

void
memclrNoHeapPointers (void *p1, uintptr len)
{
  __builtin_memset (p1, 0, len);
}
