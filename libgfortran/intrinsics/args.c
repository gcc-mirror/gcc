/* Implementation of the IARG/ARGC intrinsic(s).
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
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <sys/types.h>
#include <string.h>
#include "libgfortran.h"

void 
prefix(getarg) (GFC_INTEGER_4 *pos, char  *val, GFC_INTEGER_4 val_len)
{
  int argc;
  int arglen;
  char **argv;

  get_args (&argc, &argv);

  if (val_len < 1 || !val )
    return;   /* something is wrong , leave immediately */
  
  memset( val, ' ', val_len);

  if ((*pos) + 1 <= argc  && *pos >=0 )
    {
      arglen = strlen (argv[*pos]);
      if (arglen > val_len)
	arglen = val_len;
      memcpy (val, argv[*pos], arglen);
    }
}

GFC_INTEGER_4
prefix(iargc) ()
{
  int argc;
  char **argv;

  get_args (&argc, &argv);

  return argc;
} 
