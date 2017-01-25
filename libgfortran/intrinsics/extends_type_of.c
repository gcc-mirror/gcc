/* Implementation of the EXTENDS_TYPE_OF intrinsic.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
   Contributed by Janus Weil <janus@gcc.gnu.org>.

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


typedef struct vtype
{
  GFC_INTEGER_4 hash;
  GFC_INTEGER_4 size;
  struct vtype *extends;
}
vtype;


extern GFC_LOGICAL_4 is_extension_of (struct vtype *, struct vtype *);
export_proto(is_extension_of);


/* This is a helper function for the F2003 intrinsic EXTENDS_TYPE_OF.
   While EXTENDS_TYPE_OF accepts CLASS or TYPE arguments, this one here gets
   passed the corresponding vtabs. Each call to EXTENDS_TYPE_OF is translated
   to a call to is_extension_of.  */

GFC_LOGICAL_4
is_extension_of (struct vtype *v1, struct vtype *v2)
{
  /* Assume that only unlimited polymorphic entities will pass NULL v1 or v2
     if they are unallocated or disassociated.  */

  if (!v2)
    return 1;
  if (!v1)
    return 0;

  while (v1)
    {
      if (v1->hash == v2->hash) return 1;
      v1 = v1->extends;
    }
  return 0;
}
