/* Wrapper for the unix get{g,p,u}id functions.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfortran; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgfortran.h"

#ifdef HAVE_GETGID
GFC_INTEGER_4 prefix(getgid) (void)
{
  return (GFC_INTEGER_4) getgid ();
}
#endif

#ifdef HAVE_GETPID
GFC_INTEGER_4 prefix(getpid) (void)
{
  return (GFC_INTEGER_4) getpid ();
}
#endif

#ifdef HAVE_GETUID
GFC_INTEGER_4 prefix(getuid) (void)
{
  return (GFC_INTEGER_4) getuid ();
}
#endif
