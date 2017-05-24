/* go-strslice.c -- the go string slice function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

String
__go_string_slice (String s, intgo start, intgo end)
{
  intgo len;
  String ret;

  len = s.len;
  if (end == -1)
    end = len;
  if (start > len || end < start || end > len)
    runtime_panicstring ("string index out of bounds");
  ret.len = end - start;
  // If the length of the new string is zero, don't adjust the str
  // field.  This ensures that we don't create a pointer to the next
  // memory block, and thus keep it live unnecessarily.
  if (ret.len > 0)
    ret.str = s.str + start;
  return ret;
}
