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

/* The hash table element is represented by the following type. */

typedef const void *hash_table_entry_t;

/* Hash tables are of the following type.  The structure
   (implementation) of this type is not needed for using the hash
   tables.  All work with hash table should be executed only through
   functions mentioned below. */

typedef struct
{
  /* Current size (in entries) of the hash table */
  size_t size;
  /* Current number of elements including also deleted elements */
  size_t number_of_elements;
  /* Current number of deleted elements in the table */
  size_t number_of_deleted_elements;
  /* The following member is used for debugging. Its value is number
     of all calls of `find_hash_table_entry' for the hash table. */
  int searches;
  /* The following member is used for debugging.  Its value is number
     of collisions fixed for time of work with the hash table. */
  int collisions;
  /* Pointer to function for evaluation of hash value (any unsigned value).
     This function has one parameter of type hash_table_entry_t. */
  unsigned (*hash_function) PARAMS ((hash_table_entry_t));
  /* Pointer to function for test on equality of hash table elements (two
     parameter of type hash_table_entry_t. */
  int (*eq_function) PARAMS ((hash_table_entry_t, hash_table_entry_t));
  /* Table itself */
  hash_table_entry_t *entries;
} *hash_table_t;


/* The prototypes of the package functions. */

extern hash_table_t create_hash_table
  PARAMS ((size_t, unsigned (*) (hash_table_entry_t),
	   int (*) (hash_table_entry_t, hash_table_entry_t)));

extern void delete_hash_table PARAMS ((hash_table_t));

extern void empty_hash_table PARAMS ((hash_table_t));

extern hash_table_entry_t *find_hash_table_entry
  PARAMS ((hash_table_t, hash_table_entry_t, int));

extern void remove_element_from_hash_table_entry PARAMS ((hash_table_t,
							  hash_table_entry_t));

extern size_t hash_table_size PARAMS ((hash_table_t));

extern size_t hash_table_elements_number PARAMS ((hash_table_t));

extern int hash_table_collisions PARAMS ((hash_table_t));

extern int all_hash_table_collisions ();

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __HASHTAB_H */
