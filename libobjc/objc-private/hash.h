/* Hash tables for Objective C method dispatch.
   Copyright (C) 1993-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
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

/* You need to include this file after including objc.h  */

#ifndef __hash_INCLUDE_GNU
#define __hash_INCLUDE_GNU

#include <stddef.h>
#include <string.h>

/*
 * This data structure is used to hold items
 *  stored in a hash table.  Each node holds 
 *  a key/value pair.
 *
 * Items in the cache are really of type void *.
 */
typedef struct cache_node
{
  struct cache_node *next;	/* Pointer to next entry on the list.
				   NULL indicates end of list. */
  const void *key;		/* Key used to locate the value.  Used
				   to locate value when more than one
				   key computes the same hash
				   value. */
  void *value;			/* Value stored for the key. */
} *node_ptr;


/*
 * This data type is the function that computes a hash code given a key.
 * Therefore, the key can be a pointer to anything and the function specific
 * to the key type. 
 *
 * Unfortunately there is a mutual data structure reference problem with this
 * typedef.  Therefore, to remove compiler warnings the functions passed to
 * objc_hash_new will have to be casted to this type. 
 */
typedef unsigned int (*hash_func_type) (void *, const void *);

/*
 * This data type is the function that compares two hash keys and returns an
 * integer greater than, equal to, or less than 0, according as the first
 * parameter is lexicographically greater than, equal to, or less than the
 * second. 
 */

typedef int (*compare_func_type) (const void *, const void *);


/*
 * This data structure is the cache.
 *
 * It must be passed to all of the hashing routines
 *   (except for new).
 */
typedef struct cache
{
  /* Variables used to implement the hash itself.  */
  node_ptr *node_table; /* Pointer to an array of hash nodes.  */
  /* Variables used to track the size of the hash table so to determine
    when to resize it.  */
  unsigned int size; /* Number of buckets allocated for the hash table
			(number of array entries allocated for
			"node_table").  Must be a power of two.  */
  unsigned int used; /* Current number of entries in the hash table.  */
  unsigned int mask; /* Precomputed mask.  */

  /* Variables used to implement indexing through the hash table.  */

  unsigned int last_bucket; /* Tracks which entry in the array where
			       the last value was returned.  */
  /* Function used to compute a hash code given a key. 
     This function is specified when the hash table is created.  */
  hash_func_type    hash_func;
  /* Function used to compare two hash keys to see if they are equal.  */
  compare_func_type compare_func;
} *cache_ptr;


/* Allocate and initialize a hash table.  */ 

cache_ptr objc_hash_new (unsigned int size,
			 hash_func_type hash_func,
			 compare_func_type compare_func);
                       
/* Deallocate all of the hash nodes and the cache itself.  */

void objc_hash_delete (cache_ptr cache);

/* Add the key/value pair to the hash table.  If the
   hash table reaches a level of fullness then it will be resized. 
                                                   
   assert if the key is already in the hash.  */

void objc_hash_add (cache_ptr *cachep, const void *key, void *value);
     
/* Remove the key/value pair from the hash table.  
   assert if the key isn't in the table.  */

void objc_hash_remove (cache_ptr cache, const void *key);

/* Used to index through the hash table.  Start with NULL
   to get the first entry.
                                                  
   Successive calls pass the value returned previously.
   ** Don't modify the hash during this operation *** 
                                                  
   Cache nodes are returned such that key or value can
   be extracted.  */

node_ptr objc_hash_next (cache_ptr cache, node_ptr node);

/* Used to return a value from a hash table using a given key.  */

void *objc_hash_value_for_key (cache_ptr cache, const void *key);

/* Used to determine if the given key exists in the hash table */

BOOL objc_hash_is_key_in_hash (cache_ptr cache, const void *key);

/************************************************

        Useful hashing functions.  
        
        Declared inline for your pleasure.
        
************************************************/

/* Calculate a hash code by performing some 
   manipulation of the key pointer.  (Use the lowest bits
   except for those likely to be 0 due to alignment.)  */

static inline unsigned int
objc_hash_ptr (cache_ptr cache, const void *key)
{
  return ((size_t)key / sizeof (void *)) & cache->mask;
}


/* Calculate a hash code by iterating over a NULL 
   terminate string.  */
static inline unsigned int 
objc_hash_string (cache_ptr cache, const void *key)
{
  unsigned int ret = 0;
  unsigned int ctr = 0;
  const char *ckey = (const char *) key;
        
  while (*ckey) {
    ret ^= *ckey++ << ctr;
    ctr = (ctr + 1) % sizeof (void *);
  }

  return ret & cache->mask;
}


/* Compare two pointers for equality.  */
static inline int 
objc_compare_ptrs (const void *k1, const void *k2)
{
  return (k1 == k2);
}


/* Compare two strings.  */
static inline int 
objc_compare_strings (const void *k1, const void *k2)
{
  if (k1 == k2)
    return 1;
  else if (k1 == 0 || k2 == 0)
    return 0;
  else
    return ! strcmp ((const char *) k1, (const char *) k2);
}

#endif /* not __hash_INCLUDE_GNU */
