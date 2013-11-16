/* go-string-to-byte-array.c -- convert a string to an array of bytes in Go.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "array.h"
#include "arch.h"
#include "malloc.h"

struct __go_open_array
__go_string_to_byte_array (String str)
{
  unsigned char *data;
  struct __go_open_array ret;

  data = (unsigned char *) runtime_mallocgc (str.len, 0,
					     FlagNoScan | FlagNoZero);
  __builtin_memcpy (data, str.str, str.len);
  ret.__values = (void *) data;
  ret.__count = str.len;
  ret.__capacity = str.len;
  return ret;
}
