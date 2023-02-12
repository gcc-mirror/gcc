/* Wrapper for the unix get{g,p,u}id functions.
Copyright (C) 2004-2023 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
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

#include "libgfortran.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __MINGW32__
#define HAVE_GETPID 1
#include <process.h>
#endif

#ifdef HAVE_GETGID
extern GFC_INTEGER_4 PREFIX(getgid) (void);
export_proto_np(PREFIX(getgid));

GFC_INTEGER_4
PREFIX(getgid) (void)
{
  return getgid ();
}
#endif

#ifdef HAVE_GETPID
extern GFC_INTEGER_4 PREFIX(getpid) (void);
export_proto_np(PREFIX(getpid));

GFC_INTEGER_4
PREFIX(getpid) (void)
{
  return getpid ();
}
#endif

#ifdef HAVE_GETUID
extern GFC_INTEGER_4 PREFIX(getuid) (void);
export_proto_np(PREFIX(getuid));

GFC_INTEGER_4
PREFIX(getuid) (void)
{
  return getuid ();
}
#endif
