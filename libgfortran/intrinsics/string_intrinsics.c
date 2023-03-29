/* String intrinsics helper functions.
   Copyright (C) 2008-2023 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

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


/* Unlike what the name of this file suggests, we don't actually
   implement the Fortran intrinsics here.  At least, not with the
   names they have in the standard.  The functions here provide all
   the support we need for the standard string intrinsics, and the
   compiler translates the actual intrinsics calls to calls to
   functions in this file.  */

#include "libgfortran.h"

#include <string.h>
#include <assert.h>


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

/* Compare wide character types, which are handled internally as
   unsigned 4-byte integers.  */
int
memcmp_char4 (const void *a, const void *b, size_t len)
{
  const GFC_UINTEGER_4 *pa = a;
  const GFC_UINTEGER_4 *pb = b;
  while (len-- > 0)
    {
      if (*pa != *pb)
	return *pa < *pb ? -1 : 1;
      pa ++;
      pb ++;
    }
  return 0;
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
#undef  MEMCMP
#define MEMCMP memcmp

#include "string_intrinsics_inc.c"


#undef  CHARTYPE
#define CHARTYPE gfc_char4_t
#undef  UCHARTYPE
#define UCHARTYPE gfc_char4_t
#undef  SUFFIX
#define SUFFIX(x) x ## _char4
#undef  MEMSET
#define MEMSET memset_char4
#undef  MEMCMP
#define MEMCMP memcmp_char4

#include "string_intrinsics_inc.c"

