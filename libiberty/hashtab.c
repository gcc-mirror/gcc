/* An expandable hash tables datatype.  
   Copyright (C) 1999 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov (vmakarov@cygnus.com).

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This package implements basic hash table functionality.  It is possible
   to search for an entry, create an entry and destroy an entry.

   Elements in the table are generic pointers.

   The size of the table is not fixed; if the occupancy of the table
   grows too high the hash table will be expanded.

   The abstract data implementation is based on generalized Algorithm D
   from Knuth's book "The art of computer programming".  Hash table is
   expanded by creation of new hash table and transferring elements from
   the old table to the new table. */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#include "libiberty.h"
#include "hashtab.h"

/* The following variable is used for debugging. Its value is number
   of all calls of `find_hash_table_entry' for all hash tables. */

static int all_searches = 0;

/* The following variable is used for debugging. Its value is number
   of collisions fixed for time of work with all hash tables. */

static int all_collisions = 0;

/* The following variable is used for debugging. Its value is number
   of all table expansions fixed for time of work with all hash
   tables. */

static int all_expansions = 0;

/* This macro defines reserved value for empty table entry. */

#define EMPTY_ENTRY    NULL

/* This macro defines reserved value for table entry which contained
   a deleted element. */

#define DELETED_ENTRY  ((void *) 1)

/* The following function returns the nearest prime number which is
   greater than given source number. */

static unsigned long
higher_prime_number (number)
     unsigned long number;
{
  unsigned long i;

  for (number = (number / 2) * 2 + 3;; number += 2)
    {
      for (i = 3; i * i <= number; i += 2)
        if (number % i == 0)
          break;
      if (i * i > number)
        return number;
    }
}

/* This function creates table with length slightly longer than given
   source length.  Created hash table is initiated as empty (all the
   hash table entries are EMPTY_ENTRY).  The function returns the
   created hash table. */

hash_table_t
create_hash_table (size, hash_function, eq_function)
     size_t size;
     unsigned (*hash_function) PARAMS ((hash_table_entry_t));
     int (*eq_function) PARAMS ((hash_table_entry_t, hash_table_entry_t));
{
  hash_table_t result;

  size = higher_prime_number (size);
  result = (hash_table_t) xmalloc (sizeof (*result));
  result->entries
    = (hash_table_entry_t *) xmalloc (size * sizeof (hash_table_entry_t));
  result->size = size;
  result->hash_function = hash_function;
  result->eq_function = eq_function;
  result->number_of_elements = 0;
  result->number_of_deleted_elements = 0;
  result->searches = 0;
  result->collisions = 0;
  memset (result->entries, 0, size * sizeof (hash_table_entry_t));
  return result;
}

/* This function frees all memory allocated for given hash table.
   Naturally the hash table must already exist. */

void
delete_hash_table (htab)
     hash_table_t htab;
{
  free (htab->entries);
  free (htab);
}

/* This function clears all entries in the given hash table.  */

void
empty_hash_table (htab)
     hash_table_t htab;
{
  memset (htab->entries, 0, htab->size * sizeof (hash_table_entry_t));
}

/* The following function changes size of memory allocated for the
   entries and repeatedly inserts the table elements.  The occupancy
   of the table after the call will be about 50%.  Naturally the hash
   table must already exist.  Remember also that the place of the
   table entries is changed. */

static void
expand_hash_table (htab)
     hash_table_t htab;
{
  hash_table_t new_htab;
  hash_table_entry_t *entry_ptr;
  hash_table_entry_t *new_entry_ptr;

  new_htab = create_hash_table (htab->number_of_elements * 2,
                                htab->hash_function, htab->eq_function);
  for (entry_ptr = htab->entries; entry_ptr < htab->entries + htab->size;
       entry_ptr++)
    if (*entry_ptr != EMPTY_ENTRY && *entry_ptr != DELETED_ENTRY)
      {
        new_entry_ptr = find_hash_table_entry (new_htab, *entry_ptr, 1);
        *new_entry_ptr = (*entry_ptr);
      }
  free (htab->entries);
  *htab = (*new_htab);
  free (new_htab);
}

/* This function searches for hash table entry which contains element
   equal to given value or empty entry in which given value can be
   placed (if the element with given value does not exist in the
   table).  The function works in two regimes.  The first regime is
   used only for search.  The second is used for search and
   reservation empty entry for given value.  The table is expanded if
   occupancy (taking into accout also deleted elements) is more than
   75%.  Naturally the hash table must already exist.  If reservation
   flag is TRUE then the element with given value should be inserted
   into the table entry before another call of
   `find_hash_table_entry'. */

hash_table_entry_t *
find_hash_table_entry (htab, element, reserve)
     hash_table_t htab;
     hash_table_entry_t element;
     int reserve;
{
  hash_table_entry_t *entry_ptr;
  hash_table_entry_t *first_deleted_entry_ptr;
  unsigned index, hash_value, secondary_hash_value;

  if (htab->size * 3 <= htab->number_of_elements * 4)
    {
      all_expansions++;
      expand_hash_table (htab);
    }
  hash_value = (*htab->hash_function) (element);
  secondary_hash_value = 1 + hash_value % (htab->size - 2);
  index = hash_value % htab->size;
  htab->searches++;
  all_searches++;
  first_deleted_entry_ptr = NULL;
  for (;;htab->collisions++, all_collisions++)
    {
      entry_ptr = htab->entries + index;
      if (*entry_ptr == EMPTY_ENTRY)
        {
          if (reserve)
	    {
	      htab->number_of_elements++;
	      if (first_deleted_entry_ptr != NULL)
		{
		  entry_ptr = first_deleted_entry_ptr;
		  *entry_ptr = EMPTY_ENTRY;
		}
	    }
          break;
        }
      else if (*entry_ptr != DELETED_ENTRY)
        {
          if ((*htab->eq_function) (*entry_ptr, element))
            break;
        }
      else if (first_deleted_entry_ptr == NULL)
	first_deleted_entry_ptr = entry_ptr;
      index += secondary_hash_value;
      if (index >= htab->size)
        index -= htab->size;
    }
  return entry_ptr;
}

/* This function deletes element with given value from hash table.
   The hash table entry value will be `DELETED_ENTRY' after the
   function call.  Naturally the hash table must already exist.  Hash
   table entry for given value should be not empty (or deleted). */

void
remove_element_from_hash_table_entry (htab, element)
     hash_table_t htab;
     hash_table_entry_t element;
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (htab, element, 0);
  *entry_ptr = DELETED_ENTRY;
  htab->number_of_deleted_elements++;
}

/* This function clears a specified slot in a hash table.
   It is useful when you've already done the lookup and don't want to
   do it again.  */

void
clear_hash_table_slot (htab, slot)
     hash_table_t htab;
     hash_table_entry_t *slot;
{
  if (slot < htab->entries || slot >= htab->entries + htab->size
      || *slot == EMPTY_ENTRY || *slot == DELETED_ENTRY)
    abort ();
  *slot = DELETED_ENTRY;
  htab->number_of_deleted_elements++;
}

/* This function scans over the entire hash table calling
   CALLBACK for each live entry.  If CALLBACK returns false,
   the iteration stops.  INFO is passed as CALLBACK's second
   argument.  */

void
traverse_hash_table (htab, callback, info)
     hash_table_t htab;
     int (*callback) PARAMS ((hash_table_entry_t, void *));
     void *info;
{
  hash_table_entry_t *entry_ptr;
  for (entry_ptr = htab->entries; entry_ptr < htab->entries + htab->size;
       entry_ptr++)
    if (*entry_ptr != EMPTY_ENTRY && *entry_ptr != DELETED_ENTRY)
      if (!callback (*entry_ptr, info))
	break;
}

/* The following function returns current size of given hash table. */

size_t
hash_table_size (htab)
     hash_table_t htab;
{
  return htab->size;
}

/* The following function returns current number of elements in given
   hash table. */

size_t
hash_table_elements_number (htab)
     hash_table_t htab;
{
  return htab->number_of_elements - htab->number_of_deleted_elements;
}

/* The following function returns number of percents of fixed
   collisions during all work with given hash table. */

int
hash_table_collisions (htab)
     hash_table_t htab;
{
  int searches;

  searches = htab->searches;
  if (searches == 0)
    searches++;
  return htab->collisions * 100 / searches;
}

/* The following function returns number of percents of fixed
   collisions during all work with all hash tables. */

int
all_hash_table_collisions ()
{
  int searches;

  searches = all_searches;
  if (searches == 0)
    searches++;
  return all_collisions * 100 / searches;
}
