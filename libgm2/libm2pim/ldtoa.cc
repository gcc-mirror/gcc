/* ldtoa.c convert long double to ascii and visa versa.

Copyright (C) 2009-2022 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define GM2

#include <config.h>
#include <m2rts.h>

#define EXPORT(FUNC) m2pim ## _ldtoa_ ## FUNC
#define IMPORT(MODULE,FUNC) m2pim ## _ ## MODULE ## _ ## FUNC
#define M2EXPORT(FUNC) m2pim ## _M2_ldtoa_ ## FUNC
#define M2LIBNAME "m2pim"

#if defined(HAVE_STRINGS_H)
#include <strings.h>
#endif

#if defined(HAVE_STRING_H)
#include <string.h>
#endif

#if defined(HAVE_STDDEF_H)
/* Obtain a definition for NULL.  */
#include <stddef.h>
#endif

#if defined(HAVE_STDIO_H)
/* Obtain a definition for NULL.  */
#include <stdio.h>
#endif

#if defined(HAVE_TIME_H)
/* Obtain a definition for NULL.  */
#include <time.h>
#endif

#if defined(HAVE_STRING_H)
/* Obtain a definition for NULL.  */
#include <string.h>
#endif

#if defined(HAVE_WCHAR_H)
/* Obtain a definition for NULL.  */
#include <wchar.h>
#endif

#if defined(HAVE_STDLIB_H)
#if !defined(_ISOC99_SOURCE)
#define _ISOC99_SOURCE
#endif
#include <stdlib.h>
#endif

#if defined(HAVE_ERRNO_H)
#include <errno.h>
#endif

#if defined(HAVE_SYS_ERRNO_H)
#include <sys/errno.h>
#endif

#if defined(HAVE_STDLIB_H)
/* Obtain a prototype for free and malloc.  */
#include <stdlib.h>
#endif

#if !defined(NULL)
#define NULL (void *)0
#endif

#define MAX_FP_DIGITS 500

typedef enum Mode { maxsignicant, decimaldigits } Mode;

extern "C" int IMPORT(dtoa,calcmaxsig) (char *p, int ndigits);
extern "C" int IMPORT(dtoa,calcdecimal) (char *p, int str_size, int ndigits);
extern "C" int IMPORT(dtoa,calcsign) (char *p, int str_size);

/* maxsignicant return a string containing max(1,ndigits) significant
   digits.  The return string contains the string produced by snprintf.

   decimaldigits: return a string produced by fcvt.  The string will
   contain ndigits past the decimal point (ndigits may be negative).  */

extern "C" long double
EXPORT(strtold) (const char *s, bool *error)
{
  char *endp;
  long double d;

#if defined(HAVE_ERRNO_H)
  errno = 0;
#endif
#if defined(HAVE_STRTOLD)
  d = strtold (s, &endp);
#else
  /* Fall back to using strtod.  */
  d = (long double)strtod (s, &endp);
#endif
  if (endp != NULL && (*endp == '\0'))
#if defined(HAVE_ERRNO_H)
    *error = (errno != 0);
#else
    *error = false;
#endif
  else
    *error = true;
  return d;
}

extern "C" char *
EXPORT(ldtoa) (long double d, int mode, int ndigits, int *decpt, bool *sign)
{
  char format[50];
  char *p;
  int r;
  switch (mode)
    {

    case maxsignicant:
      ndigits += 20; /* Enough for exponent.  */
      p = (char *) malloc (ndigits);
      snprintf (format, 50, "%s%d%s", "%.", ndigits - 20, "LE");
      snprintf (p, ndigits, format, d);
      *sign = IMPORT(dtoa,calcsign) (p, ndigits);
      *decpt = IMPORT(dtoa,calcmaxsig) (p, ndigits);
      return p;
    case decimaldigits:
      p = (char *) malloc (MAX_FP_DIGITS + 20);
      snprintf (format, 50, "%s%d%s", "%.", MAX_FP_DIGITS, "LE");
      snprintf (p, MAX_FP_DIGITS + 20, format, d);
      *sign = IMPORT(dtoa,calcsign) (p, MAX_FP_DIGITS + 20);
      *decpt = IMPORT(dtoa,calcdecimal) (p, MAX_FP_DIGITS + 20, ndigits);
      return p;
    default:
      abort ();
    }
}

#if defined(GM2)
/* GNU Modula-2 linking hooks.  */

extern "C" void
M2EXPORT(init) (int, char **, char **)
{
}

extern "C" void
M2EXPORT(fini) (int, char **, char **)
{
}

extern "C" void
M2EXPORT(dep) (void)
{
}

extern "C" void __attribute__((__constructor__))
M2EXPORT(ctor) (void)
{
  m2pim_M2RTS_RegisterModule ("ldtoa", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
#endif
