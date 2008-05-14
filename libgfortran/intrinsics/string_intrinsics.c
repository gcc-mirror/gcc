/* String intrinsics helper functions.
   Copyright 2008 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

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


/* Unlike what the name of this file suggests, we don't actually
   implement the Fortran intrinsics here.  At least, not with the
   names they have in the standard.  The functions here provide all
   the support we need for the standard string intrinsics, and the
   compiler translates the actual intrinsics calls to calls to
   functions in this file.  */

#include "libgfortran.h"

#include <stdlib.h>
#include <string.h>


/* Helper function to set parts of wide strings to a constant (usually
   spaces).  */

static gfc_char4_t *
memset_char4 (gfc_char4_t *b, gfc_char4_t c, size_t len)
{
  size_t i;

  for (i = 0; i < len; i++)
    b[i] = c;

  return b;
}


/* All other functions are defined using a few generic macros in
   string_intrinsics_inc.c, so we avoid code duplication between the
   various character type kinds.  */

#undef  CHARTYPE
#define CHARTYPE char
#undef  UCHARTYPE
#define UCHARTYPE unsigned char
#undef  SUFFIX
#define SUFFIX(x) x
#undef  MEMSET
#define MEMSET memset

#include "string_intrinsics_inc.c"


#undef  CHARTYPE
#define CHARTYPE gfc_char4_t
#undef  UCHARTYPE
#define UCHARTYPE gfc_char4_t
#undef  SUFFIX
#define SUFFIX(x) x ## _char4
#undef  MEMSET
#define MEMSET memset_char4

#include "string_intrinsics_inc.c"

