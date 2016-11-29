/* go-memequal.c -- compare memory buffers for equality

   Copyright 2016 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

_Bool memequal (void *, void *, uintptr)
  __asm__ (GOSYM_PREFIX "runtime.memequal");

_Bool
memequal (void *p1, void *p2, uintptr len)
{
  return __builtin_memcmp (p1, p2, len) == 0;
}
