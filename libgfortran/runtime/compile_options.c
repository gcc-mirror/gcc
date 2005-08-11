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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"

#include "libgfortran.h"


/* Useful compile-time options will be stored in here.  */
compile_options_t compile_options;


/* Prototypes */
extern void set_std (GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(set_std);


void
set_std (GFC_INTEGER_4 warn_std, GFC_INTEGER_4 allow_std)
{
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
}
