/* Copyright (C) 2018-2019 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libgomp.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRIx64.  */
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <errno.h>

static int
gomp_gethostname (char *name, size_t len)
{
  /* On Win9x GetComputerName fails if the input size is less
     than MAX_COMPUTERNAME_LENGTH + 1.  */
  char buffer[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD size = sizeof (buffer);
  int ret = 0;

  if (!GetComputerName (buffer, &size))
    return -1;

  if ((size = strlen (buffer) + 1) > len)
    {
      errno = EINVAL;
      /* Truncate as per POSIX spec.  We do not NUL-terminate. */
      size = len;
      ret = -1;
    }
  memcpy (name, buffer, (size_t) size);

  return ret;
}

#undef gethostname
#define gethostname gomp_gethostname
#define  HAVE_GETHOSTNAME 1

#include "../../affinity-fmt.c"
