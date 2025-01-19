/* Subroutines needed for unwinding stack frames for exception handling.  */
/* Copyright (C) 1997-2025 Free Software Foundation, Inc.
   Contributed by Jason Merrill <jason@cygnus.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef _Unwind_Find_FDE
#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "dwarf2.h"
#include "unwind.h"
#define NO_BASE_OF_ENCODED_VALUE
#include "unwind-pe.h"
#include "unwind-dw2-fde.h"
#include "gthr.h"
#else
#if (defined(__GTHREAD_MUTEX_INIT) || defined(__GTHREAD_MUTEX_INIT_FUNCTION)) \
    && defined(__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4)
#define ATOMIC_FDE_FAST_PATH 1
#endif
#endif

typedef __UINTPTR_TYPE__ uintptr_type;

#ifdef ATOMIC_FDE_FAST_PATH
#include "unwind-dw2-btree.h"

static struct btree registered_frames;
static struct btree registered_objects;
static bool in_shutdown;

static void
release_registered_frames (void) __attribute__ ((destructor));
static void
release_registered_frames (void)
{
  /* Release the b-tree and all frames. Frame releases that happen later are
   * silently ignored */
  btree_destroy (&registered_frames);
  btree_destroy (&registered_objects);
  in_shutdown = true;
}

static void
get_pc_range (const struct object *ob, uintptr_type *range);

#else
/* Without fast path frame deregistration must always succeed.  */
static const int in_shutdown = 0;

/* The unseen_objects list contains objects that have been registered
   but not yet categorized in any way.  The seen_objects list has had
   its pc_begin and count fields initialized at minimum, and is sorted
   by decreasing value of pc_begin.  */
static struct object *unseen_objects;
static struct object *seen_objects;
#endif

#ifdef __GTHREAD_MUTEX_INIT
static __gthread_mutex_t object_mutex = __GTHREAD_MUTEX_INIT;
#define init_object_mutex_once()
#else
#ifdef __GTHREAD_MUTEX_INIT_FUNCTION
static __gthread_mutex_t object_mutex;

static void
init_object_mutex (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&object_mutex);
}

static void
init_object_mutex_once (void)
{
  static __gthread_once_t once = __GTHREAD_ONCE_INIT;
  __gthread_once (&once, init_object_mutex);
}
#else
/* ???  Several targets include this file with stubbing parts of gthr.h
   and expect no locking to be done.  */
#define init_object_mutex_once()
static __gthread_mutex_t object_mutex;
#endif
#endif

#ifdef ATOMIC_FDE_FAST_PATH
// Register the pc range for a given object in the lookup structure.
static void
register_pc_range_for_object (uintptr_type begin, struct object *ob)
{
  // Register the object itself to know the base pointer on deregistration.
  btree_insert (&registered_objects, begin, 1, ob);

  // Register the frame in the b-tree
  uintptr_type range[2];
  get_pc_range (ob, range);
  btree_insert (&registered_frames, range[0], range[1] - range[0], ob);
}
#endif

/* Called from crtbegin.o to register the unwind info for an object.  */

void
__register_frame_info_bases (const void *begin, struct object *ob,
			     void *tbase, void *dbase)
{
  /* If .eh_frame is empty, don't register at all.  */
  if ((const uword *) begin == 0 || *(const uword *) begin == 0)
    return;

  ob->pc_begin = (void *)-1;
  ob->tbase = tbase;
  ob->dbase = dbase;
  ob->u.single = begin;
  ob->s.i = 0;
  ob->s.b.encoding = DW_EH_PE_omit;
#ifdef DWARF2_OBJECT_END_PTR_EXTENSION
  ob->fde_end = NULL;
#endif

#ifdef ATOMIC_FDE_FAST_PATH
  register_pc_range_for_object ((uintptr_type) begin, ob);
#else
  init_object_mutex_once ();
  __gthread_mutex_lock (&object_mutex);

  ob->next = unseen_objects;
  unseen_objects = ob;

  __gthread_mutex_unlock (&object_mutex);
#endif
}

void
__register_frame_info (const void *begin, struct object *ob)
{
  __register_frame_info_bases (begin, ob, 0, 0);
}

void
__register_frame (void *begin)
{
  struct object *ob;

  /* If .eh_frame is empty, don't register at all.  */
  if (*(uword *) begin == 0)
    return;

  ob = malloc (sizeof (struct object));
  __register_frame_info (begin, ob);
}

/* Similar, but BEGIN is actually a pointer to a table of unwind entries
   for different translation units.  Called from the file generated by
   collect2.  */

void
__register_frame_info_table_bases (void *begin, struct object *ob,
				   void *tbase, void *dbase)
{
  ob->pc_begin = (void *)-1;
  ob->tbase = tbase;
  ob->dbase = dbase;
  ob->u.array = begin;
  ob->s.i = 0;
  ob->s.b.from_array = 1;
  ob->s.b.encoding = DW_EH_PE_omit;

#ifdef ATOMIC_FDE_FAST_PATH
  register_pc_range_for_object ((uintptr_type) begin, ob);
#else
  init_object_mutex_once ();
  __gthread_mutex_lock (&object_mutex);

  ob->next = unseen_objects;
  unseen_objects = ob;

  __gthread_mutex_unlock (&object_mutex);
#endif
}

void
__register_frame_info_table (void *begin, struct object *ob)
{
  __register_frame_info_table_bases (begin, ob, 0, 0);
}

void
__register_frame_table (void *begin)
{
  struct object *ob = malloc (sizeof (struct object));
  __register_frame_info_table (begin, ob);
}

/* Called from crtbegin.o to deregister the unwind info for an object.  */
/* ??? Glibc has for a while now exported __register_frame_info and
   __deregister_frame_info.  If we call __register_frame_info_bases
   from crtbegin (wherein it is declared weak), and this object does
   not get pulled from libgcc.a for other reasons, then the
   invocation of __deregister_frame_info will be resolved from glibc.
   Since the registration did not happen there, we'll die.

   Therefore, declare a new deregistration entry point that does the
   exact same thing, but will resolve to the same library as
   implements __register_frame_info_bases.  */

void *
__deregister_frame_info_bases (const void *begin)
{
  struct object *ob = 0;

  /* If .eh_frame is empty, we haven't registered.  */
  if ((const uword *) begin == 0 || *(const uword *) begin == 0)
    return ob;

#ifdef ATOMIC_FDE_FAST_PATH
  // Find the originally registered object to get the base pointer.
  ob = btree_remove (&registered_objects, (uintptr_type) begin);

  // Remove the corresponding PC range.
  if (ob)
    {
      uintptr_type range[2];
      get_pc_range (ob, range);
      if (range[0] != range[1])
	btree_remove (&registered_frames, range[0]);
    }

  // Deallocate the sort array if any.
  if (ob && ob->s.b.sorted)
    {
      free (ob->u.sort);
    }
#else
  init_object_mutex_once ();
  __gthread_mutex_lock (&object_mutex);

  struct object **p;
  for (p = &unseen_objects; *p ; p = &(*p)->next)
    if ((*p)->u.single == begin)
      {
	ob = *p;
	*p = ob->next;
	goto out;
      }

  for (p = &seen_objects; *p ; p = &(*p)->next)
    if ((*p)->s.b.sorted)
      {
	if ((*p)->u.sort->orig_data == begin)
	  {
	    ob = *p;
	    *p = ob->next;
	    free (ob->u.sort);
	    goto out;
	  }
      }
    else
      {
	if ((*p)->u.single == begin)
	  {
	    ob = *p;
	    *p = ob->next;
	    goto out;
	  }
      }

 out:
  __gthread_mutex_unlock (&object_mutex);
#endif

  // If we didn't find anything in the lookup data structures then they
  // were either already destroyed or we tried to remove an empty range.
  gcc_assert (in_shutdown || ob);
  return (void *) ob;
}

void *
__deregister_frame_info (const void *begin)
{
  return __deregister_frame_info_bases (begin);
}

void
__deregister_frame (void *begin)
{
  /* If .eh_frame is empty, we haven't registered.  */
  if (*(uword *) begin != 0)
    free (__deregister_frame_info (begin));
}


/* Like base_of_encoded_value, but take the base from a struct object
   instead of an _Unwind_Context.  */

static _Unwind_Ptr
base_from_object (unsigned char encoding, const struct object *ob)
{
  if (encoding == DW_EH_PE_omit)
    return 0;

  switch (encoding & 0x70)
    {
    case DW_EH_PE_absptr:
    case DW_EH_PE_pcrel:
    case DW_EH_PE_aligned:
      return 0;

    case DW_EH_PE_textrel:
      return (_Unwind_Ptr) ob->tbase;
    case DW_EH_PE_datarel:
      return (_Unwind_Ptr) ob->dbase;
    default:
      gcc_unreachable ();
    }
}

/* Return the FDE pointer encoding from the CIE.  */
/* ??? This is a subset of extract_cie_info from unwind-dw2.c.  */

static int
get_cie_encoding (const struct dwarf_cie *cie)
{
  const unsigned char *aug, *p;
  _Unwind_Ptr dummy;
  _uleb128_t utmp;
  _sleb128_t stmp;

  aug = cie->augmentation;
  p = aug + strlen ((const char *)aug) + 1; /* Skip the augmentation string.  */
  if (__builtin_expect (cie->version >= 4, 0))
    {
      if (p[0] != sizeof (void *) || p[1] != 0)
	return DW_EH_PE_omit;		/* We are not prepared to handle unexpected
					   address sizes or segment selectors.  */
      p += 2;				/* Skip address size and segment size.  */
    }

  if (aug[0] != 'z')
    return DW_EH_PE_absptr;

  p = read_uleb128 (p, &utmp);		/* Skip code alignment.  */
  p = read_sleb128 (p, &stmp);		/* Skip data alignment.  */
  if (cie->version == 1)		/* Skip return address column.  */
    p++;
  else
    p = read_uleb128 (p, &utmp);

  aug++;				/* Skip 'z' */
  p = read_uleb128 (p, &utmp);		/* Skip augmentation length.  */
  while (1)
    {
      /* This is what we're looking for.  */
      if (*aug == 'R')
	return *p;
      /* Personality encoding and pointer.  */
      else if (*aug == 'P')
	{
	  /* ??? Avoid dereferencing indirect pointers, since we're
	     faking the base address.  Gotta keep DW_EH_PE_aligned
	     intact, however.  */
	  p = read_encoded_value_with_base (*p & 0x7F, 0, p + 1, &dummy);
	}
      /* LSDA encoding.  */
      else if (*aug == 'L')
	p++;
      /* aarch64 b-key pointer authentication.  */
      else if (*aug == 'B')
	p++;
      /* Otherwise end of string, or unknown augmentation.  */
      else
	return DW_EH_PE_absptr;
      aug++;
    }
}

static inline int
get_fde_encoding (const struct dwarf_fde *f)
{
  return get_cie_encoding (get_cie (f));
}


/* Sorting an array of FDEs by address.
   (Ideally we would have the linker sort the FDEs so we don't have to do
   it at run time. But the linkers are not yet prepared for this.)  */

/* Comparison routines.  Three variants of increasing complexity.  */

static int
fde_unencoded_compare (struct object *ob __attribute__((unused)),
		       const fde *x, const fde *y)
{
  _Unwind_Ptr x_ptr, y_ptr;
  memcpy (&x_ptr, x->pc_begin, sizeof (_Unwind_Ptr));
  memcpy (&y_ptr, y->pc_begin, sizeof (_Unwind_Ptr));

  if (x_ptr > y_ptr)
    return 1;
  if (x_ptr < y_ptr)
    return -1;
  return 0;
}

static int
fde_single_encoding_compare (struct object *ob, const fde *x, const fde *y)
{
  _Unwind_Ptr base, x_ptr, y_ptr;

  base = base_from_object (ob->s.b.encoding, ob);
  read_encoded_value_with_base (ob->s.b.encoding, base, x->pc_begin, &x_ptr);
  read_encoded_value_with_base (ob->s.b.encoding, base, y->pc_begin, &y_ptr);

  if (x_ptr > y_ptr)
    return 1;
  if (x_ptr < y_ptr)
    return -1;
  return 0;
}

static int
fde_mixed_encoding_compare (struct object *ob, const fde *x, const fde *y)
{
  int x_encoding, y_encoding;
  _Unwind_Ptr x_ptr, y_ptr;

  x_encoding = get_fde_encoding (x);
  read_encoded_value_with_base (x_encoding, base_from_object (x_encoding, ob),
				x->pc_begin, &x_ptr);

  y_encoding = get_fde_encoding (y);
  read_encoded_value_with_base (y_encoding, base_from_object (y_encoding, ob),
				y->pc_begin, &y_ptr);

  if (x_ptr > y_ptr)
    return 1;
  if (x_ptr < y_ptr)
    return -1;
  return 0;
}

typedef int (*fde_compare_t) (struct object *, const fde *, const fde *);

// The extractor functions compute the pointer values for a block of
// fdes. The block processing hides the call overhead.

static void
fde_unencoded_extract (struct object *ob __attribute__ ((unused)),
		       _Unwind_Ptr *target, const fde **x, int count)
{
  for (int index = 0; index < count; ++index)
    memcpy (target + index, x[index]->pc_begin, sizeof (_Unwind_Ptr));
}

static void
fde_single_encoding_extract (struct object *ob, _Unwind_Ptr *target,
			     const fde **x, int count)
{
  _Unwind_Ptr base;

  base = base_from_object (ob->s.b.encoding, ob);
  for (int index = 0; index < count; ++index)
    read_encoded_value_with_base (ob->s.b.encoding, base, x[index]->pc_begin,
				  target + index);
}

static void
fde_mixed_encoding_extract (struct object *ob, _Unwind_Ptr *target,
			    const fde **x, int count)
{
  for (int index = 0; index < count; ++index)
    {
      int encoding = get_fde_encoding (x[index]);
      read_encoded_value_with_base (encoding, base_from_object (encoding, ob),
				    x[index]->pc_begin, target + index);
    }
}

typedef void (*fde_extractor_t) (struct object *, _Unwind_Ptr *, const fde **,
				 int);

// Data is sorted using radix sort if possible, using an temporary
// auxiliary data structure of the same size as the input. When running
// out of memory do in-place heap sort.

struct fde_accumulator
{
  struct fde_vector *linear;
  struct fde_vector *aux;
};

static inline int
start_fde_sort (struct fde_accumulator *accu, size_t count)
{
  size_t size;
  if (! count)
    return 0;

  size = sizeof (struct fde_vector) + sizeof (const fde *) * count;
  if ((accu->linear = malloc (size)))
    {
      accu->linear->count = 0;
      if ((accu->aux = malloc (size)))
	accu->aux->count = 0;
      return 1;
    }
  else
    return 0;
}

static inline void
fde_insert (struct fde_accumulator *accu, const fde *this_fde)
{
  if (accu->linear)
    accu->linear->array[accu->linear->count++] = this_fde;
}

#define SWAP(x,y) do { const fde * tmp = x; x = y; y = tmp; } while (0)

/* Convert a semi-heap to a heap.  A semi-heap is a heap except possibly
   for the first (root) node; push it down to its rightful place.  */

static void
frame_downheap (struct object *ob, fde_compare_t fde_compare, const fde **a,
		int lo, int hi)
{
  int i, j;

  for (i = lo, j = 2*i+1;
       j < hi;
       j = 2*i+1)
    {
      if (j+1 < hi && fde_compare (ob, a[j], a[j+1]) < 0)
	++j;

      if (fde_compare (ob, a[i], a[j]) < 0)
	{
	  SWAP (a[i], a[j]);
	  i = j;
	}
      else
	break;
    }
}

/* This is O(n log(n)).  BSD/OS defines heapsort in stdlib.h, so we must
   use a name that does not conflict.  */

static void
frame_heapsort (struct object *ob, fde_compare_t fde_compare,
		struct fde_vector *erratic)
{
  /* For a description of this algorithm, see:
     Samuel P. Harbison, Guy L. Steele Jr.: C, a reference manual, 2nd ed.,
     p. 60-61.  */
  const fde ** a = erratic->array;
  /* A portion of the array is called a "heap" if for all i>=0:
     If i and 2i+1 are valid indices, then a[i] >= a[2i+1].
     If i and 2i+2 are valid indices, then a[i] >= a[2i+2].  */
  size_t n = erratic->count;
  int m;

  /* Expand our heap incrementally from the end of the array, heapifying
     each resulting semi-heap as we go.  After each step, a[m] is the top
     of a heap.  */
  for (m = n/2-1; m >= 0; --m)
    frame_downheap (ob, fde_compare, a, m, n);

  /* Shrink our heap incrementally from the end of the array, first
     swapping out the largest element a[0] and then re-heapifying the
     resulting semi-heap.  After each step, a[0..m) is a heap.  */
  for (m = n-1; m >= 1; --m)
    {
      SWAP (a[0], a[m]);
      frame_downheap (ob, fde_compare, a, 0, m);
    }
#undef SWAP
}

// Radix sort data in V1 using V2 as aux memory. Runtime O(n).
static inline void
fde_radixsort (struct object *ob, fde_extractor_t fde_extractor,
	       struct fde_vector *v1, struct fde_vector *v2)
{
#define FANOUTBITS 8
#define FANOUT (1 << FANOUTBITS)
#define BLOCKSIZE 128
  const unsigned rounds
    = (__CHAR_BIT__ * sizeof (_Unwind_Ptr) + FANOUTBITS - 1) / FANOUTBITS;
  const fde **a1 = v1->array, **a2 = v2->array;
  _Unwind_Ptr ptrs[BLOCKSIZE + 1];
  unsigned n = v1->count;
  for (unsigned round = 0; round != rounds; ++round)
    {
      unsigned counts[FANOUT] = {0};
      unsigned violations = 0;

      // Count the number of elements per bucket and check if we are already
      // sorted.
      _Unwind_Ptr last = 0;
      for (unsigned i = 0; i < n;)
	{
	  unsigned chunk = ((n - i) <= BLOCKSIZE) ? (n - i) : BLOCKSIZE;
	  fde_extractor (ob, ptrs + 1, a1 + i, chunk);
	  ptrs[0] = last;
	  for (unsigned j = 0; j < chunk; ++j)
	    {
	      unsigned b = (ptrs[j + 1] >> (round * FANOUTBITS)) & (FANOUT - 1);
	      counts[b]++;
	      // Use summation instead of an if to eliminate branches.
	      violations += ptrs[j + 1] < ptrs[j];
	    }
	  i += chunk;
	  last = ptrs[chunk];
	}

      // Stop if we are already sorted.
      if (!violations)
	{
	  break;
	}

      // Compute the prefix sum.
      unsigned sum = 0;
      for (unsigned i = 0; i != FANOUT; ++i)
	{
	  unsigned s = sum;
	  sum += counts[i];
	  counts[i] = s;
	}

      // Place all elements.
      for (unsigned i = 0; i < n;)
	{
	  unsigned chunk = ((n - i) <= BLOCKSIZE) ? (n - i) : BLOCKSIZE;
	  fde_extractor (ob, ptrs, a1 + i, chunk);
	  for (unsigned j = 0; j < chunk; ++j)
	    {
	      unsigned b = (ptrs[j] >> (round * FANOUTBITS)) & (FANOUT - 1);
	      a2[counts[b]++] = a1[i + j];
	    }
	  i += chunk;
	}

      // Swap a1 and a2.
      const fde **tmp = a1;
      a1 = a2;
      a2 = tmp;
    }
#undef BLOCKSIZE
#undef FANOUT
#undef FANOUTBITS

  // The data is in a1 now, move in place if needed.
  if (a1 != v1->array)
    memcpy (v1->array, a1, sizeof (const fde *) * n);
}

static inline void
end_fde_sort (struct object *ob, struct fde_accumulator *accu, size_t count)
{
  gcc_assert (!accu->linear || accu->linear->count == count);

  if (accu->aux)
    {
      fde_extractor_t fde_extractor;
      if (ob->s.b.mixed_encoding)
	fde_extractor = fde_mixed_encoding_extract;
      else if (ob->s.b.encoding == DW_EH_PE_absptr)
	fde_extractor = fde_unencoded_extract;
      else
	fde_extractor = fde_single_encoding_extract;

      fde_radixsort (ob, fde_extractor, accu->linear, accu->aux);
      free (accu->aux);
    }
  else
    {
      fde_compare_t fde_compare;
      if (ob->s.b.mixed_encoding)
	fde_compare = fde_mixed_encoding_compare;
      else if (ob->s.b.encoding == DW_EH_PE_absptr)
	fde_compare = fde_unencoded_compare;
      else
	fde_compare = fde_single_encoding_compare;

      /* We've not managed to malloc an aux array,
	 so heap sort in the linear one.  */
      frame_heapsort (ob, fde_compare, accu->linear);
    }
}

/* Inspect the fde array beginning at this_fde. This
   function can be used either in query mode (RANGE is
   not null, OB is const), or in update mode (RANGE is
   null, OB is modified). In query mode the function computes
   the range of PC values and stores it in RANGE. In
   update mode it updates encoding, mixed_encoding, and pc_begin
   for OB. Return the number of fdes encountered along the way. */

static size_t
classify_object_over_fdes (struct object *ob, const fde *this_fde,
			   uintptr_type *range)
{
  const struct dwarf_cie *last_cie = 0;
  size_t count = 0;
  int encoding = DW_EH_PE_absptr;
  _Unwind_Ptr base = 0;

  for (; ! last_fde (ob, this_fde); this_fde = next_fde (this_fde))
    {
      const struct dwarf_cie *this_cie;
      _Unwind_Ptr mask, pc_begin;

      /* Skip CIEs.  */
      if (this_fde->CIE_delta == 0)
	continue;

      /* Determine the encoding for this FDE.  Note mixed encoded
	 objects for later.  */
      this_cie = get_cie (this_fde);
      if (this_cie != last_cie)
	{
	  last_cie = this_cie;
	  encoding = get_cie_encoding (this_cie);
	  if (encoding == DW_EH_PE_omit)
	    return -1;
	  base = base_from_object (encoding, ob);
	  if (!range)
	    {
	      if (ob->s.b.encoding == DW_EH_PE_omit)
		ob->s.b.encoding = encoding;
	      else if (ob->s.b.encoding != encoding)
		ob->s.b.mixed_encoding = 1;
	    }
	}

      const unsigned char *p;
      p = read_encoded_value_with_base (encoding, base, this_fde->pc_begin,
					&pc_begin);

      /* Take care to ignore link-once functions that were removed.
	 In these cases, the function address will be NULL, but if
	 the encoding is smaller than a pointer a true NULL may not
	 be representable.  Assume 0 in the representable bits is NULL.  */
      mask = size_of_encoded_value (encoding);
      if (mask < sizeof (void *))
	mask = (((_Unwind_Ptr) 1) << (mask << 3)) - 1;
      else
	mask = -1;

      if ((pc_begin & mask) == 0)
	continue;

      count += 1;
      if (range)
	{
	  _Unwind_Ptr pc_range, pc_end;
	  read_encoded_value_with_base (encoding & 0x0F, 0, p, &pc_range);
	  pc_end = pc_begin + pc_range;
	  if ((!range[0]) && (!range[1]))
	    {
	      range[0] = pc_begin;
	      range[1] = pc_end;
	    }
	  else
	    {
	      if (pc_begin < range[0])
		range[0] = pc_begin;
	      if (pc_end > range[1])
		range[1] = pc_end;
	    }
	}
      else
	{
	  if ((void *) pc_begin < ob->pc_begin)
	    ob->pc_begin = (void *) pc_begin;
	}
    }

  return count;
}

static void
add_fdes (struct object *ob, struct fde_accumulator *accu, const fde *this_fde)
{
  const struct dwarf_cie *last_cie = 0;
  int encoding = ob->s.b.encoding;
  _Unwind_Ptr base = base_from_object (ob->s.b.encoding, ob);

  for (; ! last_fde (ob, this_fde); this_fde = next_fde (this_fde))
    {
      const struct dwarf_cie *this_cie;

      /* Skip CIEs.  */
      if (this_fde->CIE_delta == 0)
	continue;

      if (ob->s.b.mixed_encoding)
	{
	  /* Determine the encoding for this FDE.  Note mixed encoded
	     objects for later.  */
	  this_cie = get_cie (this_fde);
	  if (this_cie != last_cie)
	    {
	      last_cie = this_cie;
	      encoding = get_cie_encoding (this_cie);
	      base = base_from_object (encoding, ob);
	    }
	}

      if (encoding == DW_EH_PE_absptr)
	{
	  _Unwind_Ptr ptr;
	  memcpy (&ptr, this_fde->pc_begin, sizeof (_Unwind_Ptr));
	  if (ptr == 0)
	    continue;
	}
      else
	{
	  _Unwind_Ptr pc_begin, mask;

	  read_encoded_value_with_base (encoding, base, this_fde->pc_begin,
					&pc_begin);

	  /* Take care to ignore link-once functions that were removed.
	     In these cases, the function address will be NULL, but if
	     the encoding is smaller than a pointer a true NULL may not
	     be representable.  Assume 0 in the representable bits is NULL.  */
	  mask = size_of_encoded_value (encoding);
	  if (mask < sizeof (void *))
	    mask = (((_Unwind_Ptr) 1) << (mask << 3)) - 1;
	  else
	    mask = -1;

	  if ((pc_begin & mask) == 0)
	    continue;
	}

      fde_insert (accu, this_fde);
    }
}

/* Set up a sorted array of pointers to FDEs for a loaded object.  We
   count up the entries before allocating the array because it's likely to
   be faster.  We can be called multiple times, should we have failed to
   allocate a sorted fde array on a previous occasion.  */

static inline void
init_object (struct object* ob)
{
  struct fde_accumulator accu;
  size_t count;

  count = ob->s.b.count;
  if (count == 0)
    {
      if (ob->s.b.from_array)
	{
	  fde **p = ob->u.array;
	  for (count = 0; *p; ++p)
	    {
	      size_t cur_count = classify_object_over_fdes (ob, *p, NULL);
	      if (cur_count == (size_t) -1)
		goto unhandled_fdes;
	      count += cur_count;
	    }
	}
      else
	{
	  count = classify_object_over_fdes (ob, ob->u.single, NULL);
	  if (count == (size_t) -1)
	    {
	      static const fde terminator;
	    unhandled_fdes:
	      ob->s.i = 0;
	      ob->s.b.encoding = DW_EH_PE_omit;
	      ob->u.single = &terminator;
	      return;
	    }
	}

      /* The count field we have in the main struct object is somewhat
	 limited, but should suffice for virtually all cases.  If the
	 counted value doesn't fit, re-write a zero.  The worst that
	 happens is that we re-count next time -- admittedly non-trivial
	 in that this implies some 2M fdes, but at least we function.  */
      ob->s.b.count = count;
      if (ob->s.b.count != count)
	ob->s.b.count = 0;
    }

  if (!start_fde_sort (&accu, count))
    return;

  if (ob->s.b.from_array)
    {
      fde **p;
      for (p = ob->u.array; *p; ++p)
	add_fdes (ob, &accu, *p);
    }
  else
    add_fdes (ob, &accu, ob->u.single);

  end_fde_sort (ob, &accu, count);

  /* Save the original fde pointer, since this is the key by which the
     DSO will deregister the object.  */
  accu.linear->orig_data = ob->u.single;
  ob->u.sort = accu.linear;

#ifdef ATOMIC_FDE_FAST_PATH
  // We must update the sorted bit with an atomic operation
  struct object tmp;
  tmp.s.b = ob->s.b;
  tmp.s.b.sorted = 1;
  __atomic_store (&(ob->s.b), &(tmp.s.b), __ATOMIC_RELEASE);
#else
  ob->s.b.sorted = 1;
#endif
}

#ifdef ATOMIC_FDE_FAST_PATH
/* Get the PC range for lookup */
static void
get_pc_range (const struct object *ob, uintptr_type *range)
{
  // It is safe to cast to non-const object* here as
  // classify_object_over_fdes does not modify ob in query mode.
  struct object *ncob = (struct object *) (uintptr_type) ob;
  range[0] = range[1] = 0;
  if (ob->s.b.sorted)
    {
      classify_object_over_fdes (ncob, ob->u.sort->orig_data, range);
    }
  else if (ob->s.b.from_array)
    {
      fde **p = ob->u.array;
      for (; *p; ++p)
	classify_object_over_fdes (ncob, *p, range);
    }
  else
    {
      classify_object_over_fdes (ncob, ob->u.single, range);
    }
}
#endif

/* A linear search through a set of FDEs for the given PC.  This is
   used when there was insufficient memory to allocate and sort an
   array.  */

static const fde *
linear_search_fdes (struct object *ob, const fde *this_fde, void *pc)
{
  const struct dwarf_cie *last_cie = 0;
  int encoding = ob->s.b.encoding;
  _Unwind_Ptr base = base_from_object (ob->s.b.encoding, ob);

  for (; ! last_fde (ob, this_fde); this_fde = next_fde (this_fde))
    {
      const struct dwarf_cie *this_cie;
      _Unwind_Ptr pc_begin, pc_range;

      /* Skip CIEs.  */
      if (this_fde->CIE_delta == 0)
	continue;

      if (ob->s.b.mixed_encoding)
	{
	  /* Determine the encoding for this FDE.  Note mixed encoded
	     objects for later.  */
	  this_cie = get_cie (this_fde);
	  if (this_cie != last_cie)
	    {
	      last_cie = this_cie;
	      encoding = get_cie_encoding (this_cie);
	      base = base_from_object (encoding, ob);
	    }
	}

      if (encoding == DW_EH_PE_absptr)
	{
	  const _Unwind_Ptr *pc_array = (const _Unwind_Ptr *) this_fde->pc_begin;
	  pc_begin = pc_array[0];
	  pc_range = pc_array[1];
	  if (pc_begin == 0)
	    continue;
	}
      else
	{
	  _Unwind_Ptr mask;
	  const unsigned char *p;

	  p = read_encoded_value_with_base (encoding, base,
					    this_fde->pc_begin, &pc_begin);
	  read_encoded_value_with_base (encoding & 0x0F, 0, p, &pc_range);

	  /* Take care to ignore link-once functions that were removed.
	     In these cases, the function address will be NULL, but if
	     the encoding is smaller than a pointer a true NULL may not
	     be representable.  Assume 0 in the representable bits is NULL.  */
	  mask = size_of_encoded_value (encoding);
	  if (mask < sizeof (void *))
	    mask = (((_Unwind_Ptr) 1) << (mask << 3)) - 1;
	  else
	    mask = -1;

	  if ((pc_begin & mask) == 0)
	    continue;
	}

      if ((_Unwind_Ptr) pc - pc_begin < pc_range)
	return this_fde;
    }

  return NULL;
}

/* Binary search for an FDE containing the given PC.  Here are three
   implementations of increasing complexity.  */

static inline const fde *
binary_search_unencoded_fdes (struct object *ob, void *pc)
{
  struct fde_vector *vec = ob->u.sort;
  size_t lo, hi;

  for (lo = 0, hi = vec->count; lo < hi; )
    {
      size_t i = (lo + hi) / 2;
      const fde *const f = vec->array[i];
      void *pc_begin;
      uaddr pc_range;
      memcpy (&pc_begin, (const void * const *) f->pc_begin, sizeof (void *));
      memcpy (&pc_range, (const uaddr *) f->pc_begin + 1, sizeof (uaddr));

      if (pc < pc_begin)
	hi = i;
      else if (pc >= pc_begin + pc_range)
	lo = i + 1;
      else
	return f;
    }

  return NULL;
}

static inline const fde *
binary_search_single_encoding_fdes (struct object *ob, void *pc)
{
  struct fde_vector *vec = ob->u.sort;
  int encoding = ob->s.b.encoding;
  _Unwind_Ptr base = base_from_object (encoding, ob);
  size_t lo, hi;

  for (lo = 0, hi = vec->count; lo < hi; )
    {
      size_t i = (lo + hi) / 2;
      const fde *f = vec->array[i];
      _Unwind_Ptr pc_begin, pc_range;
      const unsigned char *p;

      p = read_encoded_value_with_base (encoding, base, f->pc_begin,
					&pc_begin);
      read_encoded_value_with_base (encoding & 0x0F, 0, p, &pc_range);

      if ((_Unwind_Ptr) pc < pc_begin)
	hi = i;
      else if ((_Unwind_Ptr) pc >= pc_begin + pc_range)
	lo = i + 1;
      else
	return f;
    }

  return NULL;
}

static inline const fde *
binary_search_mixed_encoding_fdes (struct object *ob, void *pc)
{
  struct fde_vector *vec = ob->u.sort;
  size_t lo, hi;

  for (lo = 0, hi = vec->count; lo < hi; )
    {
      size_t i = (lo + hi) / 2;
      const fde *f = vec->array[i];
      _Unwind_Ptr pc_begin, pc_range;
      const unsigned char *p;
      int encoding;

      encoding = get_fde_encoding (f);
      p = read_encoded_value_with_base (encoding,
					base_from_object (encoding, ob),
					f->pc_begin, &pc_begin);
      read_encoded_value_with_base (encoding & 0x0F, 0, p, &pc_range);

      if ((_Unwind_Ptr) pc < pc_begin)
	hi = i;
      else if ((_Unwind_Ptr) pc >= pc_begin + pc_range)
	lo = i + 1;
      else
	return f;
    }

  return NULL;
}

static const fde *
search_object (struct object* ob, void *pc)
{
  /* The fast path initializes objects eagerly to avoid locking.
   * On the slow path we initialize them now */
#ifndef ATOMIC_FDE_FAST_PATH
  /* If the data hasn't been sorted, try to do this now.  We may have
     more memory available than last time we tried.  */
  if (! ob->s.b.sorted)
    {
      init_object (ob);

      /* Despite the above comment, the normal reason to get here is
	 that we've not processed this object before.  A quick range
	 check is in order.  */
      if (pc < ob->pc_begin)
	return NULL;
    }
#endif

  if (ob->s.b.sorted)
    {
      if (ob->s.b.mixed_encoding)
	return binary_search_mixed_encoding_fdes (ob, pc);
      else if (ob->s.b.encoding == DW_EH_PE_absptr)
	return binary_search_unencoded_fdes (ob, pc);
      else
	return binary_search_single_encoding_fdes (ob, pc);
    }
  else
    {
      /* Long slow laborious linear search, cos we've no memory.  */
      if (ob->s.b.from_array)
	{
	  fde **p;
	  for (p = ob->u.array; *p ; p++)
	    {
	      const fde *f = linear_search_fdes (ob, *p, pc);
	      if (f)
		return f;
	    }
	  return NULL;
	}
      else
	return linear_search_fdes (ob, ob->u.single, pc);
    }
}

#ifdef ATOMIC_FDE_FAST_PATH

// Check if the object was already initialized
static inline bool
is_object_initialized (struct object *ob)
{
  // We have to use acquire atomics for the read, which
  // is a bit involved as we read from a bitfield
  struct object tmp;
  __atomic_load (&(ob->s.b), &(tmp.s.b), __ATOMIC_ACQUIRE);
  return tmp.s.b.sorted;
}

#endif

const fde *
_Unwind_Find_FDE (void *pc, struct dwarf_eh_bases *bases)
{
  struct object *ob;
  const fde *f = NULL;

#ifdef ATOMIC_FDE_FAST_PATH
  ob = btree_lookup (&registered_frames, (uintptr_type) pc);
  if (!ob)
    return NULL;

  // Initialize the object lazily
  if (!is_object_initialized (ob))
    {
      // Check again under mutex
      init_object_mutex_once ();
      __gthread_mutex_lock (&object_mutex);

      if (!ob->s.b.sorted)
	{
	  init_object (ob);
	}

      __gthread_mutex_unlock (&object_mutex);
    }

  f = search_object (ob, pc);
#else

  init_object_mutex_once ();
  __gthread_mutex_lock (&object_mutex);

  /* Linear search through the classified objects, to find the one
     containing the pc.  Note that pc_begin is sorted descending, and
     we expect objects to be non-overlapping.  */
  for (ob = seen_objects; ob; ob = ob->next)
    if (pc >= ob->pc_begin)
      {
	f = search_object (ob, pc);
	if (f)
	  goto fini;
	break;
      }

  /* Classify and search the objects we've not yet processed.  */
  while ((ob = unseen_objects))
    {
      struct object **p;

      unseen_objects = ob->next;
      f = search_object (ob, pc);

      /* Insert the object into the classified list.  */
      for (p = &seen_objects; *p ; p = &(*p)->next)
	if ((*p)->pc_begin < ob->pc_begin)
	  break;
      ob->next = *p;
      *p = ob;

      if (f)
	goto fini;
    }

 fini:
  __gthread_mutex_unlock (&object_mutex);
#endif

  if (f)
    {
      int encoding;
      _Unwind_Ptr func;

      bases->tbase = ob->tbase;
      bases->dbase = ob->dbase;

      encoding = ob->s.b.encoding;
      if (ob->s.b.mixed_encoding)
	encoding = get_fde_encoding (f);
      read_encoded_value_with_base (encoding, base_from_object (encoding, ob),
				    f->pc_begin, &func);
      bases->func = (void *) func;
    }

  return f;
}
