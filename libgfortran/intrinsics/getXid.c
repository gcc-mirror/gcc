/* Wrapper for the unix get{g,p,u}id functions.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgfortran.h"

#ifdef __MINGW32__
#define HAVE_GETPID
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
