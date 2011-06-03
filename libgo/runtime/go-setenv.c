/* go-setenv.c -- set the C environment from Go.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

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
  vs = v.__data;
  vn = NULL;

#ifdef HAVE_SETENV

  if (ks[k.__length] != 0)
    {
      kn = __go_alloc (k.__length + 1);
      __builtin_memcpy (kn, ks, k.__length);
      ks = kn;
    }

  if (vs[v.__length] != 0)
    {
      vn = __go_alloc (v.__length + 1);
      __builtin_memcpy (vn, vs, v.__length);
      vs = vn;
    }

  setenv ((const char *) ks, (const char *) vs, 1);

#else /* !defined(HAVE_SETENV) */

  kn = malloc (k.__length + v.__length + 2);
  __builtin_memcpy (kn, ks, k.__length);
  kn[k.__length] = '=';
  __builtin_memcpy (kn + k.__length + 1, vs, v.__length);
  kn[k.__length + v.__length + 1] = '\0';
  putenv ((char *) kn);

#endif /* !defined(HAVE_SETENV) */

  if (kn != NULL)
    __go_free (kn);
  if (vn != NULL)
    __go_free (vn);
}
