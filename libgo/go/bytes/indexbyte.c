/* indexbyte.c -- implement bytes.IndexByte for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "array.h"

/* This is in C so that the compiler can optimize it appropriately.
   We deliberately don't split the stack in case it does call the
   library function, which shouldn't need much stack space.  */

int IndexByte (struct __go_open_array, char)
  asm ("bytes.IndexByte")
  __attribute__ ((no_split_stack));

int
IndexByte (struct __go_open_array s, char b)
{
  char *p;

  p = __builtin_memchr (s.__values, b, s.__count);
  if (p == NULL)
    return -1;
  return p - (char *) s.__values;
}

/* Comparison.  */

_Bool Equal (struct __go_open_array a, struct __go_open_array b)
  asm ("bytes.Equal")
  __attribute__ ((no_split_stack));

_Bool
Equal (struct __go_open_array a, struct __go_open_array b)
{
  if (a.__count != b.__count)
    return 0;
  return __builtin_memcmp (a.__values, b.__values, a.__count) == 0;
}
