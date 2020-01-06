/* Implementation of the MVBITS intrinsic
   Copyright (C) 2004-2020 Free Software Foundation, Inc.
   Contributed by Tobias Schlüter

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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

/* TODO: This should be replaced by a compiler builtin.  */

#ifndef SUB_NAME
#include <libgfortran.h>
#endif

#ifdef SUB_NAME
/* MVBITS copies LEN bits starting at bit position FROMPOS from FROM
   into TO, starting at bit position TOPOS.  */

extern void SUB_NAME (const TYPE *, const int *, const int *, TYPE *,
		      const int *);
export_proto(SUB_NAME);

void 
SUB_NAME (const TYPE *from, const int *frompos, const int *len, TYPE *to,
	  const int *topos)
{
  TYPE oldbits, newbits, lenmask;

  lenmask = (*len == sizeof (TYPE)*8) ? ~(TYPE)0 : ((TYPE)1 << *len) - 1;
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

#if defined (HAVE_GFC_INTEGER_16)
#  define TYPE GFC_INTEGER_16
#  define UTYPE GFC_UINTEGER_16
#  define SUB_NAME mvbits_i16
#  include "mvbits.c"
#  undef SUB_NAME
#  undef TYPE
#  undef UTYPE
#endif
#endif
