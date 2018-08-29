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
  // If the length of the new string is zero, the str field doesn't
  // matter, so just set it to nil.  This avoids the problem of
  // s.str + start pointing just past the end of the string,
  // which may keep the next memory block alive unnecessarily.
  if (ret.len == 0)
    ret.str = nil;
  else
    ret.str = s.str + start;
  return ret;
}
