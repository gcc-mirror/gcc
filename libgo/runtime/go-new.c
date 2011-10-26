/* go-new.c -- the generic go new() function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-alloc.h"
#include "runtime.h"
#include "arch.h"
#include "malloc.h"

void *
__go_new (uintptr_t size)
{
  return runtime_mallocgc (size, 0, 1, 1);
}

void *
__go_new_nopointers (uintptr_t size)
{
  return runtime_mallocgc (size, FlagNoPointers, 1, 1);
}
