/* go-string-to-byte-array.c -- convert a string to an array of bytes in Go.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-string.h"
#include "array.h"
#include "runtime.h"
#include "malloc.h"

struct __go_open_array
__go_string_to_byte_array (struct __go_string str)
{
  unsigned char *data;
  struct __go_open_array ret;

  data = (unsigned char *) runtime_mallocgc (str.__length, RefNoPointers, 1, 0);
  __builtin_memcpy (data, str.__data, str.__length);
  ret.__values = (void *) data;
  ret.__count = str.__length;
  ret.__capacity = str.__length;
  return ret;
}
