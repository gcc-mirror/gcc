/* Runtime conversion of strings from one character kind to another.
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
  *dst = get_mem ((l + 1) * sizeof (gfc_char4_t));

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
  *dst = get_mem ((l + 1) * sizeof (unsigned char));

  for (i = 0; i < l; i++)
    (*dst)[i] = src[i];

  (*dst)[l] = '\0';
}
