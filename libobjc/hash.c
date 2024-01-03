/* Hash tables for Objective C internal structures
   Copyright (C) 1993-2024 Free Software Foundation, Inc.

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

#include "objc-private/common.h"
#include <assert.h>               /* For assert.  */

#include "objc/runtime.h"         /* For objc_calloc.  */
#include "objc-private/hash.h"

/* These two macros determine when a hash table is full and
   by how much it should be expanded respectively.

   These equations are percentages.  */
#define FULLNESS(cache) \
   ((((cache)->size * 75) / 100) <= (cache)->used)
#define EXPANSION(cache) \
  ((cache)->size * 2)

cache_ptr
objc_hash_new (unsigned int size, hash_func_type hash_func,
	       compare_func_type compare_func)
{
  cache_ptr cache;

  /* Pass me a value greater than 0 and a power of 2.  */
  assert (size);
  assert (! (size & (size - 1)));

  /* Allocate the cache structure.  calloc insures its initialization
     for default values.  */
  cache = (cache_ptr) objc_calloc (1, sizeof (struct cache));
  assert (cache);

  /* Allocate the array of buckets for the cache.  calloc initializes
     all of the pointers to NULL.  */
  cache->node_table
    = (node_ptr *) objc_calloc (size, sizeof (node_ptr));
  assert (cache->node_table);

  cache->size  = size;

  /* This should work for all processor architectures (?).  */
  cache->mask = (size - 1);
	
  /* Store the hashing function so that codes can be computed.  */
  cache->hash_func = hash_func;

  /* Store the function that compares hash keys to determine if they
     are equal.  */
  cache->compare_func = compare_func;

  return cache;
}


void
objc_hash_delete (cache_ptr cache)
{
  node_ptr node;
  node_ptr next_node;
  unsigned int i;

  /* Purge all key/value pairs from the table.  */
  /* Step through the nodes one by one and remove every node WITHOUT
     using objc_hash_next. this makes objc_hash_delete much more
     efficient. */
  for (i = 0; i < cache->size; i++)
    {
      if ((node = cache->node_table[i]))
	{
	  /* An entry in the hash table has been found.  Now step
	     through the nodes next in the list and free them.  */
	  while ((next_node = node->next))
	    {
	      objc_hash_remove (cache,node->key);
	      node = next_node;
	    }
	  objc_hash_remove (cache,node->key);
	}
    }

  /* Release the array of nodes and the cache itself.  */
  objc_free(cache->node_table);
  objc_free(cache);
}


void
objc_hash_add (cache_ptr *cachep, const void *key, void *value)
{
  size_t indx = (*(*cachep)->hash_func) (*cachep, key);
  node_ptr node = (node_ptr) objc_calloc (1, sizeof (struct cache_node));

  assert (node);

  /* Initialize the new node.  */
  node->key    = key;
  node->value  = value;
  node->next  = (*cachep)->node_table[indx];

  /* Debugging.  Check the list for another key.  */
#ifdef DEBUG
  {
    node_ptr node1 = (*cachep)->node_table[indx];
    while (node1)
      {
	assert (node1->key != key);
	node1 = node1->next;
      }
  }
#endif

  /* Install the node as the first element on the list.  */
  (*cachep)->node_table[indx] = node;

  /* Bump the number of entries in the cache.  */
  ++(*cachep)->used;
  
  /* Check the hash table's fullness.  We're going to expand if it is
     above the fullness level.  */
  if (FULLNESS (*cachep))
    {
      /* The hash table has reached its fullness level.  Time to
	 expand it.

	 I'm using a slow method here but is built on other primitive
	 functions thereby increasing its correctness.  */
      node_ptr node1 = NULL;
      cache_ptr new = objc_hash_new (EXPANSION (*cachep),
				     (*cachep)->hash_func,
				     (*cachep)->compare_func);
      
      DEBUG_PRINTF ("Expanding cache %#x from %d to %d\n",
		    (int) *cachep, (*cachep)->size, new->size);
      
      /* Copy the nodes from the first hash table to the new one.  */
      while ((node1 = objc_hash_next (*cachep, node1)))
	objc_hash_add (&new, node1->key, node1->value);
      
      /* Trash the old cache.  */
      objc_hash_delete (*cachep);
      
      /* Return a pointer to the new hash table.  */
      *cachep = new;
    }
}

void
objc_hash_remove (cache_ptr cache, const void *key)
{
  size_t indx = (*cache->hash_func) (cache, key);
  node_ptr node = cache->node_table[indx];

  /* We assume there is an entry in the table.  Error if it is
     not.  */
  assert (node);

  /* Special case.  First element is the key/value pair to be
     removed.  */
  if ((*cache->compare_func) (node->key, key))
    {
      cache->node_table[indx] = node->next;
      objc_free(node);
    }
  else
    {
      /* Otherwise, find the hash entry.  */
      node_ptr prev = node;
      BOOL removed = NO;
      do
	{
	  if ((*cache->compare_func) (node->key, key))
	    {
	      prev->next = node->next, removed = YES;
	      objc_free(node);
	    }
	  else
	    prev = node, node = node->next;
	}
      while (!removed && node);
      assert (removed);
    }
  
  /* Decrement the number of entries in the hash table.  */
  --cache->used;
}


node_ptr
objc_hash_next (cache_ptr cache, node_ptr node)
{
  /* If the scan is being started then reset the last node visitied
     pointer and bucket index.  */
  if (!node)
    cache->last_bucket  = 0;
  
  /* If there is a node visited last then check for another entry in
     the same bucket.  Otherwise step to the next bucket.  */
  if (node)
    {
      if (node->next)
	{
	  /* There is a node which follows the last node returned.
	     Step to that node and retun it.  */
	  return node->next;
	}
      else
	++cache->last_bucket;
  }

  /* If the list isn't exhausted then search the buckets for other
     nodes.  */
  if (cache->last_bucket < cache->size)
    {
      /*  Scan the remainder of the buckets looking for an entry at
	  the head of the list.  Return the first item found.  */
      while (cache->last_bucket < cache->size)
	if (cache->node_table[cache->last_bucket])
	  return cache->node_table[cache->last_bucket];
	else
	  ++cache->last_bucket;
      
      /* No further nodes were found in the hash table.  */
      return NULL;
    }
  else
    return NULL;
}


/* Given KEY, return corresponding value for it in CACHE.  Return NULL
   if the KEY is not recorded.  */
void *
objc_hash_value_for_key (cache_ptr cache, const void *key)
{
  node_ptr node = cache->node_table[(*cache->hash_func) (cache, key)];
  void *retval = NULL;

  if (node)
    do
      {
	if ((*cache->compare_func) (node->key, key))
	  {
	    retval = node->value;
	    break;
	  }
	else
	  node = node->next;
      }
    while (! retval && node);
  
  return retval;
}

/* Given KEY, return YES if it exists in the CACHE.  Return NO if it
   does not */
BOOL
objc_hash_is_key_in_hash (cache_ptr cache, const void *key)
{
  node_ptr node = cache->node_table[(*cache->hash_func) (cache, key)];
  
  if (node)
    do
      {
	if ((*cache->compare_func)(node->key, key))
	  return YES;
	else
	  node = node->next;
      }
    while (node);

  return NO;
}
