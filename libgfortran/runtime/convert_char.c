/* Runtime conversion of strings from one character kind to another.
   Copyright (C) 2008-2014 Free Software Foundation, Inc.

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

#include "libgfortran.h"

#include <stdlib.h>
#include <string.h>


extern void convert_char1_to_char4 (gfc_char4_t **, gfc_charlen_type,
				    const unsigned char *);
export_proto(convert_char1_to_char4);

extern void convert_char4_to_char1 (unsigned char **, gfc_charlen_type,
				    const gfc_char4_t *);
export_proto(convert_char4_to_char1);


void
convert_char1_to_char4 (gfc_char4_t **dst, gfc_charlen_type len,
			const unsigned char *src)
{
  gfc_charlen_type i, l;

  l = len > 0 ? len : 0;
  *dst = xmalloc ((l + 1) * sizeof (gfc_char4_t));

  for (i = 0; i < l; i++)
    (*dst)[i] = src[i];

  (*dst)[l] = '\0';
}


void
convert_char4_to_char1 (unsigned char **dst, gfc_charlen_type len,
			const gfc_char4_t *src)
{
  gfc_charlen_type i, l;

  l = len > 0 ? len : 0;
  *dst = xmalloc ((l + 1) * sizeof (unsigned char));

  for (i = 0; i < l; i++)
    (*dst)[i] = src[i];

  (*dst)[l] = '\0';
}
