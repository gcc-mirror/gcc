/* Implementation of the MVBITS intrinsic
   Copyright (C) 2004 Free Software Foundation, Inc.
   Contributed by Tobias Schlüter

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
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* TODO: This should be replaced by a compiler builtin.  */

#ifndef SUB_NAME
#include <libgfortran.h>
#endif

#ifdef SUB_NAME
/* MVBITS copies LEN bits starting at bit position FROMPOS from FROM
   into TO, starting at bit position TOPOS.  */

extern void SUB_NAME (const TYPE *, const GFC_INTEGER_4 *,
		      const GFC_INTEGER_4 *, TYPE *, const GFC_INTEGER_4 *);
export_proto(SUB_NAME);

void 
SUB_NAME (const TYPE *from, const GFC_INTEGER_4 *frompos,
          const GFC_INTEGER_4 *len, TYPE *to, const GFC_INTEGER_4 *topos)
{
  TYPE oldbits, newbits, lenmask;

  lenmask = (*len == sizeof (TYPE)*8) ? ~(TYPE)0 : (1 << *len) - 1;
  newbits = (((UTYPE)(*from) >> *frompos) & lenmask) << *topos;
  oldbits = *to & (~(lenmask << *topos));

  *to = newbits | oldbits;
}
#endif

#ifndef SUB_NAME
#  define TYPE GFC_INTEGER_1
#  define UTYPE GFC_UINTEGER_1
#  define SUB_NAME mvbits_i1
#  include "mvbits.c"
#  undef SUB_NAME
#  undef TYPE
#  undef UTYPE
 
#  define TYPE GFC_INTEGER_2
#  define UTYPE GFC_UINTEGER_2
#  define SUB_NAME mvbits_i2
#  include "mvbits.c"
#  undef SUB_NAME
#  undef TYPE
#  undef UTYPE
 
#  define TYPE GFC_INTEGER_4
#  define UTYPE GFC_UINTEGER_4
#  define SUB_NAME mvbits_i4
#  include "mvbits.c"
#  undef SUB_NAME
#  undef TYPE
#  undef UTYPE

#  define TYPE GFC_INTEGER_8
#  define UTYPE GFC_UINTEGER_8
#  define SUB_NAME mvbits_i8
#  include "mvbits.c"
#  undef SUB_NAME
#  undef TYPE
#  undef UTYPE
#endif
