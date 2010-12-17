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
  asm ("libgo_bytes.bytes.IndexByte")
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
