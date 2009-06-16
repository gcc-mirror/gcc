/* Vector API for GNU compiler.
   Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>

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
#include "ggc.h"
#include "vec.h"
#include "coretypes.h"
#include "toplev.h"
#include "hashtab.h"

struct vec_prefix 
{
  unsigned num;
  unsigned alloc;
  void *vec[1];
};


#ifdef GATHER_STATISTICS

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
static void
register_overhead (struct vec_prefix *ptr, size_t size,
		   const char *name, int line, const char *function)
{
  struct vec_descriptor *loc = vec_descriptor (name, line, function);
  struct ptr_hash_entry *p = XNEW (struct ptr_hash_entry);
  PTR *slot;

  p->ptr = ptr;
  p->loc = loc;
  p->allocated = size;
  if (!ptr_hash)
    ptr_hash = htab_create (10, hash_ptr, eq_ptr, NULL);
  slot = htab_find_slot_with_hash (ptr_hash, ptr, htab_hash_pointer (ptr), INSERT);
  gcc_assert (!*slot);
  *slot = p;

  loc->allocated += size;
  if (loc->peak < loc->allocated)
    loc->peak += loc->allocated;
  loc->times++;
}

/* Notice that the pointer has been freed.  */
static void
free_overhead (struct vec_prefix *ptr)
{
  PTR *slot = htab_find_slot_with_hash (ptr_hash, ptr, htab_hash_pointer (ptr),
					NO_INSERT);
  struct ptr_hash_entry *p = (struct ptr_hash_entry *) *slot;
  p->loc->allocated -= p->allocated;
  htab_clear_slot (ptr_hash, slot);
  free (p);
}

void
vec_heap_free (void *ptr)
{
  free_overhead ((struct vec_prefix *)ptr);
  free (ptr);
}
#endif

/* Calculate the new ALLOC value, making sure that RESERVE slots are
   free.  If EXACT grow exactly, otherwise grow exponentially.  */

static inline unsigned
calculate_allocation (const struct vec_prefix *pfx, int reserve, bool exact)
{
  unsigned alloc = 0;
  unsigned num = 0;

  gcc_assert (reserve >= 0);

  if (pfx)
    {
      alloc = pfx->alloc;
      num = pfx->num;
    }
  else if (!reserve)
    /* If there's no prefix, and we've not requested anything, then we
       will create a NULL vector.  */
    return 0;
  
  /* We must have run out of room.  */
  gcc_assert (alloc - num < (unsigned) reserve);
  
  if (exact)
    /* Exact size.  */
    alloc = num + reserve;
  else
    {
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
      if (alloc < num + reserve)
	alloc = num + reserve;
    }
  return alloc;
}

/* Ensure there are at least RESERVE free slots in VEC.  If EXACT grow
   exactly, else grow exponentially.  As a special case, if VEC is
   NULL and RESERVE is 0, no vector will be created.  The vector's
   trailing array is at VEC_OFFSET offset and consists of ELT_SIZE
   sized elements.  */

static void *
vec_gc_o_reserve_1 (void *vec, int reserve, size_t vec_offset, size_t elt_size,
		    bool exact MEM_STAT_DECL)
{
  struct vec_prefix *pfx = (struct vec_prefix *) vec;
  unsigned alloc = calculate_allocation (pfx, reserve, exact);
  
  if (!alloc)
    {
      if (pfx)
        ggc_free (pfx);
      return NULL;
    }
  
  vec = ggc_realloc_stat (vec, vec_offset + alloc * elt_size PASS_MEM_STAT);
  ((struct vec_prefix *)vec)->alloc = alloc;
  if (!pfx)
    ((struct vec_prefix *)vec)->num = 0;
  
  return vec;
}

/* Ensure there are at least RESERVE free slots in VEC, growing
   exponentially.  If RESERVE < 0 grow exactly, else grow
   exponentially.  As a special case, if VEC is NULL, and RESERVE is
   0, no vector will be created. */

void *
vec_gc_p_reserve (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_gc_o_reserve_1 (vec, reserve,
			     offsetof (struct vec_prefix, vec),
			     sizeof (void *), false
			     PASS_MEM_STAT);
}

/* Ensure there are at least RESERVE free slots in VEC, growing
   exactly.  If RESERVE < 0 grow exactly, else grow exponentially.  As
   a special case, if VEC is NULL, and RESERVE is 0, no vector will be
   created. */

void *
vec_gc_p_reserve_exact (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_gc_o_reserve_1 (vec, reserve,
			     offsetof (struct vec_prefix, vec),
			     sizeof (void *), true
			     PASS_MEM_STAT);
}

/* As for vec_gc_p_reserve, but for object vectors.  The vector's
   trailing array is at VEC_OFFSET offset and consists of ELT_SIZE
   sized elements.  */

void *
vec_gc_o_reserve (void *vec, int reserve, size_t vec_offset, size_t elt_size
		  MEM_STAT_DECL)
{
  return vec_gc_o_reserve_1 (vec, reserve, vec_offset, elt_size, false
			     PASS_MEM_STAT);
}

/* As for vec_gc_p_reserve_exact, but for object vectors.  The
   vector's trailing array is at VEC_OFFSET offset and consists of
   ELT_SIZE sized elements.  */

void *
vec_gc_o_reserve_exact (void *vec, int reserve, size_t vec_offset,
			size_t elt_size MEM_STAT_DECL)
{
  return vec_gc_o_reserve_1 (vec, reserve, vec_offset, elt_size, true
			     PASS_MEM_STAT);
}

/* As for vec_gc_o_reserve_1, but for heap allocated vectors.  */

static void *
vec_heap_o_reserve_1 (void *vec, int reserve, size_t vec_offset,
		      size_t elt_size, bool exact MEM_STAT_DECL)
{
  struct vec_prefix *pfx = (struct vec_prefix *) vec;
  unsigned alloc = calculate_allocation (pfx, reserve, exact);

  if (!alloc)
    {
      if (pfx)
        vec_heap_free (pfx);
      return NULL;
    }

#ifdef GATHER_STATISTICS
  if (vec)
    free_overhead (pfx);
#endif
  
  vec = xrealloc (vec, vec_offset + alloc * elt_size);
  ((struct vec_prefix *)vec)->alloc = alloc;
  if (!pfx)
    ((struct vec_prefix *)vec)->num = 0;
#ifdef GATHER_STATISTICS
  if (vec)
    register_overhead ((struct vec_prefix *)vec,
    		       vec_offset + alloc * elt_size PASS_MEM_STAT);
#endif
  
  return vec;
}

/* As for vec_gc_p_reserve, but for heap allocated vectors.  */

void *
vec_heap_p_reserve (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_heap_o_reserve_1 (vec, reserve,
			       offsetof (struct vec_prefix, vec),
			       sizeof (void *), false
			       PASS_MEM_STAT);
}

/* As for vec_gc_p_reserve_exact, but for heap allocated vectors.  */

void *
vec_heap_p_reserve_exact (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_heap_o_reserve_1 (vec, reserve,
			       offsetof (struct vec_prefix, vec),
			       sizeof (void *), true
			       PASS_MEM_STAT);
}

/* As for vec_gc_o_reserve, but for heap allocated vectors.  */

void *
vec_heap_o_reserve (void *vec, int reserve, size_t vec_offset, size_t elt_size
		    MEM_STAT_DECL)
{
  return vec_heap_o_reserve_1 (vec, reserve, vec_offset, elt_size, false
			       PASS_MEM_STAT);
}

/* As for vec_gc_o_reserve_exact, but for heap allocated vectors.  */

void *
vec_heap_o_reserve_exact (void *vec, int reserve, size_t vec_offset,
			  size_t elt_size MEM_STAT_DECL)
{
  return vec_heap_o_reserve_1 (vec, reserve, vec_offset, elt_size, true
			       PASS_MEM_STAT);
}

/* Stack vectors are a little different.  VEC_alloc turns into a call
   to vec_stack_p_reserve_exact1 and passes in space allocated via a
   call to alloca.  We record that pointer so that we know that we
   shouldn't free it.  If the vector is resized, we resize it on the
   heap.  We record the pointers in a vector and search it in LIFO
   order--i.e., we look for the newest stack vectors first.  We don't
   expect too many stack vectors at any one level, and searching from
   the end should normally be efficient even if they are used in a
   recursive function.  */

typedef void *void_p;
DEF_VEC_P(void_p);
DEF_VEC_ALLOC_P(void_p,heap);

static VEC(void_p,heap) *stack_vecs;

/* Allocate a vector which uses alloca for the initial allocation.
   SPACE is space allocated using alloca, ALLOC is the number of
   entries allocated.  */

void *
vec_stack_p_reserve_exact_1 (int alloc, void *space)
{
  struct vec_prefix *pfx = (struct vec_prefix *) space;

  VEC_safe_push (void_p, heap, stack_vecs, space);

  pfx->num = 0;
  pfx->alloc = alloc;

  return space;
}

/* Grow a vector allocated using alloca.  When this happens, we switch
   back to heap allocation.  We remove the vector from stack_vecs, if
   it is there, since we no longer need to avoid freeing it.  */

static void *
vec_stack_o_reserve_1 (void *vec, int reserve, size_t vec_offset,
		       size_t elt_size, bool exact MEM_STAT_DECL)
{
  bool found;
  unsigned int ix;
  void *newvec;

  found = false;
  for (ix = VEC_length (void_p, stack_vecs); ix > 0; --ix)
    {
      if (VEC_index (void_p, stack_vecs, ix - 1) == vec)
	{
	  VEC_unordered_remove (void_p, stack_vecs, ix - 1);
	  found = true;
	  break;
	}
    }

  if (!found)
    {
      /* VEC is already on the heap.  */
      return vec_heap_o_reserve_1 (vec, reserve, vec_offset, elt_size,
				   exact PASS_MEM_STAT);
    }

  /* Move VEC to the heap.  */
  reserve += ((struct vec_prefix *) vec)->num;
  newvec = vec_heap_o_reserve_1 (NULL, reserve, vec_offset, elt_size,
				 exact PASS_MEM_STAT);
  if (newvec && vec)
    {
      ((struct vec_prefix *) newvec)->num = ((struct vec_prefix *) vec)->num;
      memcpy (((struct vec_prefix *) newvec)->vec,
	      ((struct vec_prefix *) vec)->vec,
	      ((struct vec_prefix *) vec)->num * elt_size);
    }
  return newvec;
}

/* Grow a vector allocated on the stack.  */

void *
vec_stack_p_reserve (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_stack_o_reserve_1 (vec, reserve,
				offsetof (struct vec_prefix, vec),
				sizeof (void *), false
				PASS_MEM_STAT);
}

/* Exact version of vec_stack_p_reserve.  */

void *
vec_stack_p_reserve_exact (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_stack_o_reserve_1 (vec, reserve,
				offsetof (struct vec_prefix, vec),
				sizeof (void *), true
				PASS_MEM_STAT);
}

/* Like vec_stack_p_reserve, but for objects.  */

void *
vec_stack_o_reserve (void *vec, int reserve, size_t vec_offset,
		     size_t elt_size MEM_STAT_DECL)
{
  return vec_stack_o_reserve_1 (vec, reserve, vec_offset, elt_size, false
				PASS_MEM_STAT);
}

/* Like vec_stack_p_reserve_exact, but for objects.  */

void *
vec_stack_o_reserve_exact (void *vec, int reserve, size_t vec_offset,
			    size_t elt_size MEM_STAT_DECL)
{
  return vec_stack_o_reserve_1 (vec, reserve, vec_offset, elt_size, true
				PASS_MEM_STAT);
}

/* Free a vector allocated on the stack.  Don't actually free it if we
   find it in the hash table.  */

void
vec_stack_free (void *vec)
{
  unsigned int ix;

  for (ix = VEC_length (void_p, stack_vecs); ix > 0; --ix)
    {
      if (VEC_index (void_p, stack_vecs, ix - 1) == vec)
	{
	  VEC_unordered_remove (void_p, stack_vecs, ix - 1);
	  return;
	}
    }

  /* VEC was not on the list of vecs allocated on the stack, so it
     must be allocated on the heap.  */
  vec_heap_free (vec);
}

#if ENABLE_CHECKING
/* Issue a vector domain error, and then fall over.  */

void
vec_assert_fail (const char *op, const char *struct_name,
		 const char *file, unsigned int line, const char *function)
{
  internal_error ("vector %s %s domain error, in %s at %s:%u",
		  struct_name, op, function, trim_filename (file), line);
}
#endif

#ifdef GATHER_STATISTICS
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
#endif
void
dump_vec_loc_statistics (void)
{
#ifdef GATHER_STATISTICS
  int nentries = 0;
  char s[4096];
  size_t allocated = 0;
  size_t times = 0;
  int i;

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
#endif
}
