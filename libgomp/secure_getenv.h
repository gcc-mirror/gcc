/* Copyright (C) 2017-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef _SECURE_GETENV_H
#define _SECURE_GETENV_H 1

/* Secure getenv() which returns NULL if running as SUID/SGID.  */
#ifndef HAVE_SECURE_GETENV
#if defined (HAVE_UNISTD_H) && defined (HAVE_GETUID) \
  && defined (HAVE_GETEUID) && defined (HAVE_GETGID) \
  && defined (HAVE_GETEGID)

#include <unistd.h>

#if SUPPORTS_WEAKREF && defined (HAVE___SECURE_GETENV)
static char* weak_secure_getenv (const char*)
  __attribute__((__weakref__("__secure_getenv")));
#endif

/* Implementation of secure_getenv() for targets where it is not provided but
   we have at least means to test real and effective IDs.  */

static inline char *
secure_getenv (const char *name)
{
#if SUPPORTS_WEAKREF && defined (HAVE___SECURE_GETENV)
  if (weak_secure_getenv)
    return weak_secure_getenv (name);
#endif

  if ((getuid () == geteuid ()) && (getgid () == getegid ()))
    return getenv (name);
  else
    return NULL;
}
#else
#define secure_getenv getenv
#endif
#endif

#endif /* _SECURE_GETENV_H.  */
