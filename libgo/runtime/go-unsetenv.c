/* go-unsetenv.c -- unset an environment variable from Go.

   Copyright 2015 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include <stddef.h>
#include <stdlib.h>

#include "runtime.h"

/* Unset an environment variable from Go.  This is called by
   syscall.Unsetenv.  */

void unsetenv_c (String) __asm__ (GOSYM_PREFIX "syscall.unsetenv_c");

void
unsetenv_c (String k)
{
  const byte *ks;
  unsigned char *kn;

  ks = k.str;
  if (ks == NULL)
    ks = (const byte *) "";
  kn = NULL;

#ifdef HAVE_UNSETENV

  if (ks[k.len] != 0)
    {
      kn = malloc (k.len + 1);
      if (kn == NULL)
	runtime_throw ("out of malloc memory");
      __builtin_memcpy (kn, ks, k.len);
      ks = kn;
    }

  unsetenv ((const char *) ks);

#endif /* !defined(HAVE_UNSETENV) */

  if (kn != NULL)
    free (kn);
}
