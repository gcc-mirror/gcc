/* go-getgoroot.c -- getgoroot function for runtime package.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdlib.h>

#include "go-string.h"

struct __go_string getgoroot (void) asm ("runtime.getgoroot");

struct __go_string
getgoroot ()
{
  const char *p;
  struct __go_string ret;

  p = getenv ("GOROOT");
  ret.__data = (const unsigned char *) p;
  if (ret.__data == NULL)
    ret.__length = 0;
  else
    ret.__length = __builtin_strlen (p);
  return ret;
}
