/* go-memclr.c -- clear a memory buffer

   Copyright 2016 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

void memclrNoHeapPointers(void *, uintptr)
  __asm__ (GOSYM_PREFIX "runtime.memclrNoHeapPointers")
  __attribute__ ((no_split_stack));

void
memclrNoHeapPointers(void *p1, uintptr len)
{
  const uintptr ptr_size = sizeof(p1);
  uintptr rem,drem,i;
  uintptr offset;
  volatile uintptr *vp;

  if (len == 0) {
    return;
  }
  rem = len;

  offset = (uintptr)p1 % ptr_size;
  if (rem < ptr_size || offset > 0) {
    // This memset is OK since it can't contain
    // an pointer aligned pointer.
    __builtin_memset(p1, 0, rem);
    return;
  }

  drem = rem / ptr_size;

  vp = (volatile uintptr*)(p1);
  // Without the use of volatile here, the compiler
  // might convert the loop into a memset.
  for (i=0; i<drem; i++) {
    *vp = 0;
    vp++;
    rem -= ptr_size;
  }
  // Clear any remaining bytes.
  if (rem > 0) {
    p1 = (void*)((char*)p1 + ptr_size*drem);
    __builtin_memset(p1, 0, rem);
  }
}
