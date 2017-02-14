/* go-setenv.c -- set the C environment from Go.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include <stddef.h>
#include <stdlib.h>

#include "runtime.h"

/* Set the C environment from Go.  This is called by syscall.Setenv.  */

void setenv_c (String, String) __asm__ (GOSYM_PREFIX "syscall.setenv_c");

void
setenv_c (String k, String v)
{
  const byte *ks;
  unsigned char *kn;
  const byte *vs;
  unsigned char *vn;

  ks = k.str;
  if (ks == NULL)
    ks = (const byte *) "";
  kn = NULL;

  vs = v.str;
  if (vs == NULL)
    vs = (const byte *) "";
  vn = NULL;

#ifdef HAVE_SETENV

  if (ks[k.len] != 0)
    {
      kn = malloc (k.len + 1);
      if (kn == NULL)
	runtime_throw ("out of malloc memory");
      __builtin_memcpy (kn, ks, k.len);
      kn[k.len] = '\0';
      ks = kn;
    }

  if (vs[v.len] != 0)
    {
      vn = malloc (v.len + 1);
      if (vn == NULL)
	runtime_throw ("out of malloc memory");
      __builtin_memcpy (vn, vs, v.len);
      vn[v.len] = '\0';
      vs = vn;
    }

  setenv ((const char *) ks, (const char *) vs, 1);

#else /* !defined(HAVE_SETENV) */

  len = k.len + v.len + 2;
  kn = malloc (len);
  if (kn == NULL)
    runtime_throw ("out of malloc memory");
  __builtin_memcpy (kn, ks, k.len);
  kn[k.len] = '=';
  __builtin_memcpy (kn + k.len + 1, vs, v.len);
  kn[k.len + v.len + 1] = '\0';
  putenv ((char *) kn);
  kn = NULL; /* putenv takes ownership of the string.  */

#endif /* !defined(HAVE_SETENV) */

  if (kn != NULL)
    free (kn);
  if (vn != NULL)
    free (vn);
}
