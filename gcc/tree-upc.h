/* tree-upc.h: UPC language-specific tree node support.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_TREE_UPC_H
#define GCC_TREE_UPC_H 1

/* Non-zero if the UPC blocking factor is 0.  */
#define TYPE_HAS_UPC_BLOCK_FACTOR_0(NODE)  \
  TYPE_CHECK (NODE)->base.u.bits.upc_block_factor_0

/* Non-zero if the UPC blocking factor is greater than 1.
   In this case, the blocking factor value is stored in a hash table.  */
#define TYPE_HAS_UPC_BLOCK_FACTOR_X(NODE) \
  TYPE_CHECK (NODE)->base.u.bits.upc_block_factor_x

/* Non-zero if the UPC blocking factor is not equal to 1 (the default).  */
#define TYPE_HAS_UPC_BLOCK_FACTOR(NODE) \
  (TYPE_UPC_SHARED(NODE) \
   && (TYPE_HAS_UPC_BLOCK_FACTOR_0 (NODE) \
       || TYPE_HAS_UPC_BLOCK_FACTOR_X (NODE)))

/* Return the UPC blocking factor of the type given by NODE..
   The default block factor is one.  The additional flag bits
   over-ride the default.  */
#define TYPE_UPC_BLOCK_FACTOR(NODE) \
  (TYPE_UPC_SHARED (NODE) \
    ? (TYPE_HAS_UPC_BLOCK_FACTOR_0 (NODE) ? size_zero_node \
      : TYPE_HAS_UPC_BLOCK_FACTOR_X (NODE) ? upc_block_factor_lookup (NODE) \
      : NULL_TREE) \
    : NULL_TREE)

/* Set the UPC block factor in the type described by NODE.
   For a zero blocking factor set TYPE_UPC_BLOCK_FACTOR_0 (NODE). 
   For a blocking factor greater than 1, insert the value
   into a hash table indexed by NODE, and then set the
   flag TYPE_UPC_BLOCK_FACTOR_X (NODE).  */
#define SET_TYPE_UPC_BLOCK_FACTOR(NODE, VAL) \
  do { \
    if (TYPE_UPC_SHARED (NODE)) \
      { \
	TYPE_HAS_UPC_BLOCK_FACTOR_0 (NODE) = 0; \
	TYPE_HAS_UPC_BLOCK_FACTOR_X (NODE) = 0; \
	if (VAL) \
	  { \
	    gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (VAL))); \
	    if (!integer_onep (VAL)) \
	      { \
		if (integer_zerop (VAL)) \
		  TYPE_HAS_UPC_BLOCK_FACTOR_0 (NODE) = 1; \
		else \
		  { \
		    TYPE_HAS_UPC_BLOCK_FACTOR_X (NODE) = 1; \
		    upc_block_factor_insert (NODE, VAL); \
		  } \
	      } \
          } \
      } \
    else \
      gcc_assert (!VAL); \
  } while (0)

extern void upc_block_factor_insert (tree, tree);
extern tree upc_block_factor_lookup (const_tree);
extern tree build_upc_unshared_type (tree);
extern void upc_block_factor_lookup_init (void);
extern tree upc_get_block_factor (const tree);

#endif /* !GCC_TREE_UPC_H */
