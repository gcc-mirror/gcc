/* go-byte-array-to-string.c -- convert an array of bytes to a string in Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-string.h"
#include "runtime.h"
#include "arch.h"
#include "malloc.h"

struct __go_string
__go_byte_array_to_string (const void* p, int len)
{
  const unsigned char *bytes;
  unsigned char *retdata;
  struct __go_string ret;

  bytes = (const unsigned char *) p;
  retdata = runtime_mallocgc (len, FlagNoPointers, 1, 0);
  __builtin_memcpy (retdata, bytes, len);
  ret.__data = retdata;
  ret.__length = len;
  return ret;
}
