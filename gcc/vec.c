/* Vector API for GNU compiler.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>
   Re-implemented in C++ by Diego Novillo <dnovillo@google.com>

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

/* This file is compiled twice: once for the generator programs
   once for the compiler.  */
#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif

#include "system.h"
#include "coretypes.h"
#include "ggc.h"
#include "vec.h"
#include "diagnostic-core.h"
#include "hashtab.h"

/* vNULL is an empty type with a template cast operation that returns
   a zero-initialized vec<T, A, L> instance.  Use this when you want
   to assign nil values to new vec instances or pass a nil vector as
   a function call argument.

   We use this technique because vec<T, A, L> must be PODs (they are
   stored in unions and passed in vararg functions), this means that
   they cannot have ctors/dtors.  */
vnull vNULL;


/* Store information about each particular vector.  */
struct vec_descriptor
{
  const char *function;
  const char *file;
  int line;
  size_t allocated;
  size_t times;
  size_t peak;
};


/* Hashtable mapping vec addresses to descriptors.  */
static htab_t vec_desc_hash;

/* Hashtable helpers.  */
static hashval_t
hash_descriptor (const void *p)
{
  const struct vec_descriptor *const d =
    (const struct vec_descriptor *) p;
  return htab_hash_pointer (d->file) + d->line;
}
static int
eq_descriptor (const void *p1, const void *p2)
{
  const struct vec_descriptor *const d = (const struct vec_descriptor *) p1;
  const struct vec_descriptor *const l = (const struct vec_descriptor *) p2;
  return d->file == l->file && d->function == l->function && d->line == l->line;
}

/* Hashtable converting address of allocated field to loc descriptor.  */
static htab_t ptr_hash;
struct ptr_hash_entry
{
  void *ptr;
  struct vec_descriptor *loc;
  size_t allocated;
};

/* Hash table helpers functions.  */
static hashval_t
hash_ptr (const void *p)
{
  const struct ptr_hash_entry *const d = (const struct ptr_hash_entry *) p;

  return htab_hash_pointer (d->ptr);
}

static int
eq_ptr (const void *p1, const void *p2)
{
  const struct ptr_hash_entry *const p = (const struct ptr_hash_entry *) p1;

  return (p->ptr == p2);
}

/* Return descriptor for given call site, create new one if needed.  */
static struct vec_descriptor *
vec_descriptor (const char *name, int line, const char *function)
{
  struct vec_descriptor loc;
  struct vec_descriptor **slot;

  loc.file = name;
  loc.line = line;
  loc.function = function;
  if (!vec_desc_hash)
    vec_desc_hash = htab_create (10, hash_descriptor, eq_descriptor, NULL);

  slot = (struct vec_descriptor **) htab_find_slot (vec_desc_hash, &loc,
						    INSERT);
  if (*slot)
    return *slot;
  *slot = XCNEW (struct vec_descriptor);
  (*slot)->file = name;
  (*slot)->line = line;
  (*slot)->function = function;
  (*slot)->allocated = 0;
  (*slot)->peak = 0;
  return *slot;
}

/* Account the overhead.  */

void
vec_prefix::register_overhead (size_t size, const char *name, int line,
			       const char *function)
{
  struct vec_descriptor *loc = vec_descriptor (name, line, function);
  struct ptr_hash_entry *p = XNEW (struct ptr_hash_entry);
  PTR *slot;

  p->ptr = this;
  p->loc = loc;
  p->allocated = size;
  if (!ptr_hash)
    ptr_hash = htab_create (10, hash_ptr, eq_ptr, NULL);
  slot = htab_find_slot_with_hash (ptr_hash, this, htab_hash_pointer (this),
				   INSERT);
  gcc_assert (!*slot);
  *slot = p;

  loc->allocated += size;
  if (loc->peak < loc->allocated)
    loc->peak += loc->allocated;
  loc->times++;
}


/* Notice that the memory allocated for the vector has been freed.  */

void
vec_prefix::release_overhead (void)
{
  PTR *slot = htab_find_slot_with_hash (ptr_hash, this,
					htab_hash_pointer (this),
					NO_INSERT);
  struct ptr_hash_entry *p = (struct ptr_hash_entry *) *slot;
  p->loc->allocated -= p->allocated;
  htab_clear_slot (ptr_hash, slot);
  ::free (p);
}


/* Calculate the number of slots to reserve a vector, making sure that
   it is of at least DESIRED size by growing ALLOC exponentially.  */

unsigned
vec_prefix::calculate_allocation_1 (unsigned alloc, unsigned desired)
{
  /* We must have run out of room.  */
  gcc_assert (alloc < desired);

  /* Exponential growth. */
  if (!alloc)
    alloc = 4;
  else if (alloc < 16)
    /* Double when small.  */
    alloc = alloc * 2;
  else
    /* Grow slower when large.  */
    alloc = (alloc * 3 / 2);

  /* If this is still too small, set it to the right size. */
  if (alloc < desired)
    alloc = desired;
  return alloc;
}


/* Helper for qsort; sort descriptors by amount of memory consumed.  */

static int
cmp_statistic (const void *loc1, const void *loc2)
{
  const struct vec_descriptor *const l1 =
    *(const struct vec_descriptor *const *) loc1;
  const struct vec_descriptor *const l2 =
    *(const struct vec_descriptor *const *) loc2;
  long diff;
  diff = l1->allocated - l2->allocated;
  if (!diff)
    diff = l1->peak - l2->peak;
  if (!diff)
    diff = l1->times - l2->times;
  return diff > 0 ? 1 : diff < 0 ? -1 : 0;
}


/* Collect array of the descriptors from hashtable.  */

static struct vec_descriptor **loc_array;
static int
add_statistics (void **slot, void *b)
{
  int *n = (int *)b;
  loc_array[*n] = (struct vec_descriptor *) *slot;
  (*n)++;
  return 1;
}

/* Dump per-site memory statistics.  */

void
dump_vec_loc_statistics (void)
{
  int nentries = 0;
  char s[4096];
  size_t allocated = 0;
  size_t times = 0;
  int i;

  if (! GATHER_STATISTICS)
    return;

  loc_array = XCNEWVEC (struct vec_descriptor *, vec_desc_hash->n_elements);
  fprintf (stderr, "Heap vectors:\n");
  fprintf (stderr, "\n%-48s %10s       %10s       %10s\n",
	   "source location", "Leak", "Peak", "Times");
  fprintf (stderr, "-------------------------------------------------------\n");
  htab_traverse (vec_desc_hash, add_statistics, &nentries);
  qsort (loc_array, nentries, sizeof (*loc_array), cmp_statistic);
  for (i = 0; i < nentries; i++)
    {
      struct vec_descriptor *d = loc_array[i];
      allocated += d->allocated;
      times += d->times;
    }
  for (i = 0; i < nentries; i++)
    {
      struct vec_descriptor *d = loc_array[i];
      const char *s1 = d->file;
      const char *s2;
      while ((s2 = strstr (s1, "gcc/")))
	s1 = s2 + 4;
      sprintf (s, "%s:%i (%s)", s1, d->line, d->function);
      s[48] = 0;
      fprintf (stderr, "%-48s %10li:%4.1f%% %10li      %10li:%4.1f%% \n", s,
	       (long)d->allocated,
	       (d->allocated) * 100.0 / allocated,
	       (long)d->peak,
	       (long)d->times,
	       (d->times) * 100.0 / times);
    }
  fprintf (stderr, "%-48s %10ld                        %10ld\n",
	   "Total", (long)allocated, (long)times);
  fprintf (stderr, "\n%-48s %10s       %10s       %10s\n",
	   "source location", "Leak", "Peak", "Times");
  fprintf (stderr, "-------------------------------------------------------\n");
}
