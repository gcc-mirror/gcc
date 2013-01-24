/* go-getgoroot.c -- getgoroot function for runtime package.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdlib.h>

#include "runtime.h"

String getgoroot (void) __asm__ (GOSYM_PREFIX "runtime.getgoroot");

String
getgoroot ()
{
  const char *p;
  String ret;

  p = getenv ("GOROOT");
  ret.str = (const byte *) p;
  if (ret.str == NULL)
    ret.len = 0;
  else
    ret.len = __builtin_strlen (p);
  return ret;
}
