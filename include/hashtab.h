/* An expandable hash tables datatype.  
   Copyright (C) 1999 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov (vmakarov@cygnus.com).

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* This package implements basic hash table functionality.  It is possible
   to search for an entry, create an entry and destroy an entry.

   Elements in the table are generic pointers.

   The size of the table is not fixed; if the occupancy of the table
   grows too high the hash table will be expanded.

   The abstract data implementation is based on generalized Algorithm D
   from Knuth's book "The art of computer programming".  Hash table is
   expanded by creation of new hash table and transferring elements from
   the old table to the new table.  */

#ifndef __HASHTAB_H__
#define __HASHTAB_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <ansidecl.h>

/* Callback function pointer types.  */

/* Calculate hash of a table entry.  */
typedef unsigned int (*htab_hash) PARAMS ((const void *));

/* Compare a table entry with a possible entry.  The entry already in
   the table always comes first.  */
typedef int (*htab_eq) PARAMS ((const void *, const void *));

/* Cleanup function called whenever a live element is removed from
   the hash table.  */
typedef void (*htab_del) PARAMS ((void *));
  
/* Function called by htab_traverse for each live element.  The first
   arg is the element, the second arg is the auxiliary pointer handed
   to htab_traverse.  Return 1 to continue scan, 0 to stop.  */
typedef int (*htab_trav) PARAMS ((void *, void *));

/* Hash tables are of the following type.  The structure
   (implementation) of this type is not needed for using the hash
   tables.  All work with hash table should be executed only through
   functions mentioned below. */

struct htab
{
  /* Pointer to hash function.  */
  htab_hash hash_f;

  /* Pointer to comparison function.  */
  htab_eq eq_f;

  /* Pointer to cleanup function.  */
  htab_del del_f;

  /* Table itself.  */
  void **entries;

  /* Current size (in entries) of the hash table */
  size_t size;

  /* Current number of elements including also deleted elements */
  size_t n_elements;

  /* Current number of deleted elements in the table */
  size_t n_deleted;

  /* The following member is used for debugging. Its value is number
     of all calls of `htab_find_slot' for the hash table. */
  unsigned int searches;

  /* The following member is used for debugging.  Its value is number
     of collisions fixed for time of work with the hash table. */
  unsigned int collisions;
};

typedef struct htab *htab_t;

/* The prototypes of the package functions. */

extern htab_t	htab_create	PARAMS ((size_t, htab_hash,
					 htab_eq, htab_del));
extern void	htab_delete	PARAMS ((htab_t));
extern void	htab_empty	PARAMS ((htab_t));

extern void    *htab_find	PARAMS ((htab_t, const void *));
extern void   **htab_find_slot	PARAMS ((htab_t, const void *, int));
extern void	htab_clear_slot	PARAMS ((htab_t, void **));
extern void	htab_remove_elt	PARAMS ((htab_t, void *));

extern void	htab_traverse	PARAMS ((htab_t, htab_trav, void *));

extern size_t	htab_size	PARAMS ((htab_t));
extern size_t	htab_elements	PARAMS ((htab_t));
extern double	htab_collisions	PARAMS ((htab_t));

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __HASHTAB_H */
