/* Handling of compile-time options that influence the library.
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

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

You should have received a copy of the GNU General Public License
along with libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"

#include "libgfortran.h"


/* Useful compile-time options will be stored in here.  */
compile_options_t compile_options;


/* Prototypes */
extern void set_std (GFC_INTEGER_4, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(set_std);


void
set_std (GFC_INTEGER_4 warn_std, GFC_INTEGER_4 allow_std,
	 GFC_INTEGER_4 pedantic)
{
  compile_options.pedantic = pedantic;
  compile_options.warn_std = warn_std;
  compile_options.allow_std = allow_std;
}


/* Default values for the compile-time options.  Keep in sync with
   gcc/fortran/options.c (gfc_init_options).  */
void
init_compile_options (void)
{
  compile_options.warn_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
    | GFC_STD_F2003 | GFC_STD_LEGACY;
  compile_options.allow_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
    | GFC_STD_F2003 | GFC_STD_F95 | GFC_STD_F77 | GFC_STD_GNU | GFC_STD_LEGACY;
  compile_options.pedantic = 0;
}

/* Function called by the front-end to tell us the
   default for unformatted data conversion.  */

extern void set_convert (int);
export_proto (set_convert);

void
set_convert (int conv)
{
  compile_options.convert = conv;
}

extern void set_record_marker (int);
export_proto (set_record_marker);


void
set_record_marker (int val)
{

  switch(val)
    {
    case 4:
      compile_options.record_marker = sizeof (GFC_INTEGER_4);
      break;

    case 8:
      compile_options.record_marker = sizeof (GFC_INTEGER_8);
      break;

    default:
      runtime_error ("Invalid value for record marker");
      break;
    }
}

extern void set_max_subrecord_length (int);
export_proto (set_max_subrecord_length);

void set_max_subrecord_length(int val)
{
  if (val > GFC_MAX_SUBRECORD_LENGTH || val < 1)
    {
      runtime_error ("Invalid value for maximum subrecord length");
      return;
    }

  compile_options.max_subrecord_length = val;
}
