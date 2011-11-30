/* go-strslice.c -- the go string slice function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-string.h"
#include "go-panic.h"
#include "runtime.h"
#include "arch.h"
#include "malloc.h"

struct __go_string
__go_string_slice (struct __go_string s, int start, int end)
{
  int len;
  struct __go_string ret;

  len = s.__length;
  if (end == -1)
    end = len;
  if (start > len || end < start || end > len)
    runtime_panicstring ("string index out of bounds");
  ret.__data = s.__data + start;
  ret.__length = end - start;
  return ret;
}
