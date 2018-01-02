/* A representation of vector permutation indices.
   Copyright (C) 2017 Free Software Foundation, Inc.

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

#ifndef GCC_VEC_PERN_INDICES_H
#define GCC_VEC_PERN_INDICES_H 1

/* This class represents a constant permutation vector, such as that used
   as the final operand to a VEC_PERM_EXPR.  */
class vec_perm_indices : public auto_vec<unsigned short, 32>
{
  typedef unsigned short element_type;
  typedef auto_vec<element_type, 32> parent_type;

public:
  vec_perm_indices () {}
  vec_perm_indices (unsigned int nunits) : parent_type (nunits) {}

  void new_expanded_vector (const vec_perm_indices &, unsigned int);

  bool all_in_range_p (element_type, element_type) const;

private:
  vec_perm_indices (const vec_perm_indices &);
};

/* Temporary.  */
typedef vec_perm_indices vec_perm_builder;
typedef vec_perm_indices auto_vec_perm_indices;

bool tree_to_vec_perm_builder (vec_perm_builder *, tree);
rtx vec_perm_indices_to_rtx (machine_mode, const vec_perm_indices &);

#endif
