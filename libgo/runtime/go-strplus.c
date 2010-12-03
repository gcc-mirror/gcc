/* go-strplus.c -- the go string append function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-string.h"
#include "runtime.h"
#include "malloc.h"

struct __go_string
__go_string_plus (struct __go_string s1, struct __go_string s2)
{
  int len;
  unsigned char *retdata;
  struct __go_string ret;

  if (s1.__length == 0)
    return s2;
  else if (s2.__length == 0)
    return s1;

  len = s1.__length + s2.__length;
  retdata = runtime_mallocgc (len, RefNoPointers, 1, 0);
  __builtin_memcpy (retdata, s1.__data, s1.__length);
  __builtin_memcpy (retdata + s1.__length, s2.__data, s2.__length);
  ret.__data = retdata;
  ret.__length = len;
  return ret;
}
