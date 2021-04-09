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

#if !defined(__PPC64__)
  __builtin_memset(p1, 0, len);
#else
  int64 rem,drem,i;
  uint64 offset;
  volatile uint64 *vp;

  if (len == 0) {
    return;
  }
  rem = len;

  offset = (uint64)p1 % 8;
  // This memset is OK since it can't contain
  // an 8 byte aligned pointer.
  if ((rem < 8) || (offset > 0 && offset+rem <= 16)) {
    __builtin_memset(p1, 0, rem);
    return;
  }
  // Move initial bytes to get to 8 byte boundary
  if (offset > 0) {
    __builtin_memset(p1, 0, 8-offset);
    p1 = (void*)((char*)p1+8-offset);
    rem -= 8-offset;
  }

  // If at least 8 bytes left, clear
  drem = rem>>3;

  vp = (volatile uint64*)(p1);
  // Without the use of volatile here, the compiler
  // might convert the loop into a memset.
  for (i=0; i<drem; i++) {
    *vp = 0;
    vp++;
    rem -= 8;
  }
  p1 = (void*)((char*)p1 + 8*drem);
  // Clear any remaining
  if (rem > 0) {
    __builtin_memset (p1, 0, rem);
  }
#endif
}
