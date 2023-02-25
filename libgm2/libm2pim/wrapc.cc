/* wrapc.c provide access to miscellaneous C library functions.

Copyright (C) 2005-2022 Free Software Foundation, Inc.
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

#include <config.h>
#include <m2rts.h>

#define EXPORT(FUNC) m2pim ## _wrapc_ ## FUNC
#define M2EXPORT(FUNC) m2pim ## _M2_wrapc_ ## FUNC
#define M2LIBNAME "m2pim"

#if defined(HAVE_MATH_H)
#include <math.h>
#endif

#if defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(HAVE_SYS_STAT_H)
#include <sys/stat.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#if defined(HAVE_TIME_H)
#include <time.h>
#endif

/* Define FALSE if one hasn't already been defined.  */

#if !defined(FALSE)
#define FALSE (1 == 0)
#endif

/* Define a generic NULL if one hasn't already been defined.  */

#if !defined(NULL)
#define NULL 0
#endif

/* strtime returns the address of a string which describes the
   local time.  */

extern "C" char *
EXPORT(strtime) (void)
{
#if defined(HAVE_CTIME)
  time_t clock = time (NULL);
  char *string = ctime (&clock);

  string[24] = (char)0;

  return string;
#else
  return "";
#endif
}

extern "C" int
EXPORT(filesize) (int f, unsigned int *low, unsigned int *high)
{
#if defined(HAVE_SYS_STAT_H) && defined(HAVE_STRUCT_STAT)
  struct stat s;
  int res = fstat (f, (struct stat *)&s);

  if (res == 0)
    {
      *low = (unsigned int)s.st_size;
      *high = (unsigned int)(s.st_size >> (sizeof (unsigned int) * 8));
    }
  return res;
#else
  return -1;
#endif
}

/* filemtime returns the mtime of a file, f.  */

extern "C" int
EXPORT(filemtime) (int f)
{
#if defined(HAVE_SYS_STAT_H) && defined(HAVE_STRUCT_STAT)
  struct stat s;

  if (fstat (f, (struct stat *)&s) == 0)
    return s.st_mtime;
  else
    return -1;
#else
  return -1;
#endif
}

/* fileinode returns the inode associated with a file, f.  */

#if defined(HAVE_SYS_STAT_H) && defined(HAVE_STRUCT_STAT)
extern "C" ino_t
EXPORT(fileinode) (int f, unsigned int *low, unsigned int *high)
{
  struct stat s;

  if (fstat (f, (struct stat *)&s) == 0)
    {
      *low = (unsigned int)s.st_ino;
      if ((sizeof (s.st_ino) == (sizeof (unsigned int))))
	*high = 0;
      else
	*high = (unsigned int)(s.st_ino >> (sizeof (unsigned int) * 8));
      return 0;
    }
  else
    return -1;
}
#else
extern "C" int
EXPORT(fileinode) (int f, unsigned int *low, unsigned int *high)
{
  *low = 0;
  *high = 0;
  return -1;
}
#endif

/* getrand returns a random number between 0..n-1.  */

extern "C" int
EXPORT(getrand) (int n)
{
  return rand () % n;
}

#if defined(HAVE_PWD_H)
#include <pwd.h>

extern "C" char *
EXPORT(getusername) (void)
{
  return getpwuid (getuid ())->pw_gecos;
}

/* getnameuidgid fills in the, uid, and, gid, which represents
   user, name.  */

extern "C" void
EXPORT(getnameuidgid) (char *name, int *uid, int *gid)
{
  struct passwd *p = getpwnam (name);

  if (p == NULL)
    {
      *uid = -1;
      *gid = -1;
    }
  else
    {
      *uid = p->pw_uid;
      *gid = p->pw_gid;
    }
}
#else
extern "C" char *
EXPORT(getusername) (void)
{
  return "unknown";
}

extern "C" void
EXPORT(getnameuidgid) (char *name, int *uid, int *gid)
{
  *uid = -1;
  *gid = -1;
}
#endif

extern "C" int
EXPORT(signbit) (double r)
{
#if defined(HAVE_SIGNBIT)

  /* signbit is a macro which tests its argument against sizeof(float),
     sizeof(double).  */
  return signbit (r);
#else
  return FALSE;
#endif
}

extern "C" int
EXPORT(signbitl) (long double r)
{
#if defined(HAVE_SIGNBITL)

  /* signbit is a macro which tests its argument against sizeof(float),
     sizeof(double).  */
  return signbitl (r);
#else
  return FALSE;
#endif
}

extern "C" int
EXPORT(signbitf) (float r)
{
#if defined(HAVE_SIGNBITF)

  /* signbit is a macro which tests its argument against sizeof(float),
     sizeof(double).  */
  return signbitf (r);
#else
  return FALSE;
#endif
}

/* isfinite provide non builtin alternative to the gcc builtin
   isfinite.  Returns 1 if x is finite and 0 if it is not.  */

extern "C" int
EXPORT(isfinite) (double x)
{
#if defined(FP_NAN) && defined(FP_INFINITE)
  return (fpclassify (x) != FP_NAN && fpclassify (x) != FP_INFINITE);
#else
  return FALSE;
#endif
}

/* isfinitel provide non builtin alternative to the gcc builtin
   isfinite.  Returns 1 if x is finite and 0 if it is not.  */

extern "C" int
EXPORT(isfinitel) (long double x)
{
#if defined(FP_NAN) && defined(FP_INFINITE)
  return (fpclassify (x) != FP_NAN && fpclassify (x) != FP_INFINITE);
#else
  return FALSE;
#endif
}

/* isfinitef provide non builtin alternative to the gcc builtin
   isfinite.  Returns 1 if x is finite and 0 if it is not.  */

extern "C" int
EXPORT(isfinitef) (float x)
{
#if defined(FP_NAN) && defined(FP_INFINITE)
  return (fpclassify (x) != FP_NAN && fpclassify (x) != FP_INFINITE);
#else
  return FALSE;
#endif
}

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
  m2pim_M2RTS_RegisterModule ("wrapc", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
