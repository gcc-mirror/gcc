/* go-setenv.c -- set the C environment from Go.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>
#include <stdlib.h>

#include "go-alloc.h"
#include "go-string.h"

/* Set the C environment from Go.  This is called by os.Setenv.  */

void setenv_c (struct __go_string, struct __go_string)
  __asm__ ("libgo_os.os.setenv_c");

void
setenv_c (struct __go_string k, struct __go_string v)
{
  const unsigned char *ks;
  unsigned char *kn;
  const unsigned char *vs;
  unsigned char *vn;

  ks = k.__data;
  kn = NULL;
  if (ks[k.__length] != 0)
    {
      kn = __go_alloc (k.__length + 1);
      __builtin_memcpy (kn, k.__data, k.__length);
      ks = kn;
    }

  vs = v.__data;
  vn = NULL;
  if (vs[v.__length] != 0)
    {
      vn = __go_alloc (v.__length + 1);
      __builtin_memcpy (vn, v.__data, v.__length);
      vs = vn;
    }

  setenv ((const char *) ks, (const char *) vs, 1);

  if (kn != NULL)
    __go_free (kn);
  if (vn != NULL)
    __go_free (vn);
}
