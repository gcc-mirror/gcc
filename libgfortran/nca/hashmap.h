#ifndef HASHMAP_H

#include "shared_memory.h"
#include "allocator.h"

#include <stdint.h>
#include <stddef.h>


/* Data structures and variables:

   memid is a unique identifier for the coarray, the address of its
   descriptor (which is unique in the program).  */
typedef intptr_t memid;

typedef struct {
  shared_mem_ptr data;
  size_t size;
  int bitnum;
} hashmap_shared;

typedef struct hashmap
{
  hashmap_shared *s;
  shared_memory *sm;
  allocator *a;
} hashmap;

typedef struct {
  shared_mem_ptr p;
  size_t size;
  ssize_t res_offset;
} hashmap_search_result;

void hashmap_init (hashmap *, hashmap_shared *, allocator *a, shared_memory *);

/* Look up memid in the hashmap. The result can be inspected via the
   hm_search_result_* functions.  */

hashmap_search_result hashmap_get (hashmap *, memid);

/* Given a search result, returns the size.  */
size_t hm_search_result_size (hashmap_search_result *);

/* Given a search result, returns the pointer.  */
shared_mem_ptr hm_search_result_ptr (hashmap_search_result *);

/* Given a search result, returns whether something was found.  */
bool hm_search_result_contains (hashmap_search_result *);

/* Sets the hashmap entry for memid to shared_mem_ptr and
   size_t. Optionally, if a hashmap_search_result is supplied, it is
   used to make the lookup faster. */

void hashmap_set (hashmap *, memid, hashmap_search_result *, shared_mem_ptr p,
                  size_t);

/* Increments the hashmap entry for memid. Optionally, if a
   hashmap_search_result is supplied, it is used to make the lookup
   faster. */

void hashmap_inc (hashmap *, memid, hashmap_search_result *);

/* Same, but decrement.  */
int hashmap_dec (hashmap *, memid, hashmap_search_result *);

void dump_hm (hashmap *hm);

#define HASHMAP_H
#endif
