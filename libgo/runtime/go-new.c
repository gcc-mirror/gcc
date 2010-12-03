/* go-new.c -- the generic go new() function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-alloc.h"
#include "runtime.h"
#include "malloc.h"

void *
__go_new (size_t size)
{
  return __go_alloc (size);
}

void *
__go_new_nopointers (size_t size)
{
  return runtime_mallocgc (size, RefNoPointers, 1, 1);
}
