/* VEC types for basic types of the intermediate representations.
   Copyright (C) 2010 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_VECIR_H
#define GCC_VECIR_H

#ifndef GCC_CORETYPES_H
#error "vecir.h must be included after coretypes.h"
#endif

/* A varray of trees.  */
DEF_VEC_P(tree);
DEF_VEC_ALLOC_P(tree,gc);
DEF_VEC_ALLOC_P(tree,heap);

/* A varray of gimple statements.  */
DEF_VEC_P(gimple);
DEF_VEC_ALLOC_P(gimple,heap);
DEF_VEC_ALLOC_P(gimple,gc);

/* A varray of pointers to gimple statements.  */
typedef gimple *gimple_p;
DEF_VEC_P(gimple_p);
DEF_VEC_ALLOC_P(gimple_p,heap);

/* A varray gimple statement sequences.  */
DEF_VEC_P(gimple_seq);
DEF_VEC_ALLOC_P(gimple_seq,gc);
DEF_VEC_ALLOC_P(gimple_seq,heap);

/* A varray of RTX objects.  */
DEF_VEC_P(rtx);
DEF_VEC_ALLOC_P(rtx,heap);
DEF_VEC_ALLOC_P(rtx,gc);

#endif /* GCC_VECIR_H */
