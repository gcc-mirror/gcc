/* Get common system includes and various definitions and declarations
   based on target macros.
   Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef __GCC_TSYSTEM_H__
#define __GCC_TSYSTEM_H__

/* GCC supplies this header. */
#include <stddef.h>

#ifdef inhibit_libc

#ifndef malloc
extern void *malloc (size_t);
#endif

#ifndef free
extern void free (void *);
#endif

#ifndef atexit
extern int atexit (void (*)(void));
#endif

#else /* ! inhibit_libc */
/* We disable this when inhibit_libc, so that gcc can still be built without
   needing header files first.  */
/* ??? This is not a good solution, since prototypes may be required in
   some cases for correct code.  */

/* GCC supplies this header. */
#include <stdarg.h>

/* All systems have this header. */
#include <stdio.h>

/* All systems have this header. */
#include <sys/types.h>

/* All systems have this header. */
#include <errno.h>

#ifndef errno
extern int errno;
#endif

#if defined(POSIX) || defined(USG)
#include <string.h>
#endif

/* GCC (fixproto) guarantees these system headers exist. */
#include <stdlib.h>
#include <unistd.h>

/* GCC supplies this header. */
#include <limits.h>

#if defined(POSIX) || defined(USG)
#include <time.h>
#endif

#endif /* inhibit_libc */

/* Define a generic NULL if one hasn't already been defined.  */
#ifndef NULL
#define NULL 0
#endif

#endif /* __GCC_TSYSTEM_H__ */
