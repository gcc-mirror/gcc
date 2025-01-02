/* Vector API for GNU compiler.
   Copyright (C) 2004-2025 Free Software Foundation, Inc.
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
#include "hash-table.h"
#include "selftest.h"
#ifdef GENERATOR_FILE
#include "errors.h"
#else
#include "input.h"
#include "diagnostic-core.h"
#endif

/* Vector memory usage.  */
class vec_usage: public mem_usage
{
public:
  /* Default constructor.  */
  vec_usage (): m_items (0), m_items_peak (0), m_element_size (0) {}

  /* Constructor.  */
  vec_usage (size_t allocated, size_t times, size_t peak,
	     size_t items, size_t items_peak, size_t element_size)
    : mem_usage (allocated, times, peak),
    m_items (items), m_items_peak (items_peak),
    m_element_size (element_size) {}

  /* Sum the usage with SECOND usage.  */
  vec_usage
  operator+ (const vec_usage &second)
  {
    return vec_usage (m_allocated + second.m_allocated,
		      m_times + second.m_times,
		      m_peak + second.m_peak,
		      m_items + second.m_items,
		      m_items_peak + second.m_items_peak, 0);
  }

  /* Dump usage coupled to LOC location, where TOTAL is sum of all rows.  */
  inline void
  dump (mem_location *loc, mem_usage &total) const
  {
    char s[4096];
    sprintf (s, "%s:%i (%s)", loc->get_trimmed_filename (),
	     loc->m_line, loc->m_function);

    s[48] = '\0';

    fprintf (stderr,
	     "%-48s %10" PRIu64 PRsa (10) ":%4.1f%%" PRsa (9) "%10" PRIu64
	     ":%4.1f%%" PRsa (10) PRsa (10) "\n",
	     s,
	     (uint64_t)m_element_size,
	     SIZE_AMOUNT (m_allocated),
	     m_allocated * 100.0 / total.m_allocated,
	     SIZE_AMOUNT (m_peak), (uint64_t)m_times,
	     m_times * 100.0 / total.m_times,
	     SIZE_AMOUNT (m_items), SIZE_AMOUNT (m_items_peak));
  }

  /* Dump footer.  */
  inline void
  dump_footer ()
  {
    fprintf (stderr, "%s" PRsa (64) PRsa (25) PRsa (16) "\n",
	     "Total", SIZE_AMOUNT (m_allocated),
	     SIZE_AMOUNT (m_times), SIZE_AMOUNT (m_items));
  }

  /* Dump header with NAME.  */
  static inline void
  dump_header (const char *name)
  {
    fprintf (stderr, "%-48s %10s%11s%16s%10s%17s%11s\n", name, "sizeof(T)",
	     "Leak", "Peak", "Times", "Leak items", "Peak items");
  }

  /* Current number of items allocated.  */
  size_t m_items;
  /* Peak value of number of allocated items.  */
  size_t m_items_peak;
  /* Size of element of the vector.  */
  size_t m_element_size;
};

/* Vector memory description.  */
static mem_alloc_description <vec_usage> vec_mem_desc;

/* Account the overhead.  */

void
vec_prefix::register_overhead (void *ptr, size_t elements,
			       size_t element_size MEM_STAT_DECL)
{
  vec_mem_desc.register_descriptor (ptr, VEC_ORIGIN, false
				    FINAL_PASS_MEM_STAT);
  vec_usage *usage
    = vec_mem_desc.register_instance_overhead (elements * element_size, ptr);
  usage->m_element_size = element_size;
  usage->m_items += elements;
  if (usage->m_items_peak < usage->m_items)
    usage->m_items_peak = usage->m_items;
}

/* Notice that the memory allocated for the vector has been freed.  */

void
vec_prefix::release_overhead (void *ptr, size_t size, size_t elements,
			      bool in_dtor MEM_STAT_DECL)
{
  if (!vec_mem_desc.contains_descriptor_for_instance (ptr))
    vec_mem_desc.register_descriptor (ptr, VEC_ORIGIN,
				      false FINAL_PASS_MEM_STAT);
  vec_usage *usage = vec_mem_desc.release_instance_overhead (ptr, size,
							     in_dtor);
  usage->m_items -= elements;
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

/* Dump per-site memory statistics.  */

void
dump_vec_loc_statistics (void)
{
  vec_mem_desc.dump (VEC_ORIGIN);
}

#if CHECKING_P
/* Report qsort comparator CMP consistency check failure with P1, P2, P3 as
   witness elements.  */
ATTRIBUTE_NORETURN ATTRIBUTE_COLD
static void
qsort_chk_error (const void *p1, const void *p2, const void *p3,
		 sort_r_cmp_fn *cmp, void *data)
{
  if (!p3)
    {
      int r1 = cmp (p1, p2, data), r2 = cmp (p2, p1, data);
      error ("qsort comparator not anti-symmetric: %d, %d", r1, r2);
    }
  else if (p1 == p2)
    {
      int r = cmp (p1, p3, data);
      error ("qsort comparator non-negative on sorted output: %d", r);
    }
  else
    {
      int r1 = cmp (p1, p2, data);
      int r2 = cmp (p2, p3, data);
      int r3 = cmp (p1, p3, data);
      error ("qsort comparator not transitive: %d, %d, %d", r1, r2, r3);
    }
  internal_error ("qsort checking failed");
}

/* Verify anti-symmetry and transitivity for comparator CMP on sorted array
   of N SIZE-sized elements pointed to by BASE.  */
void
qsort_chk (void *base, size_t n, size_t size, sort_r_cmp_fn *cmp, void *data)
{
#if 0
#define LIM(n) (n)
#else
  /* Limit overall time complexity to O(n log n).  */
#define LIM(n) ((n) <= 16 ? (n) : 12 + floor_log2 (n))
#endif
#define ELT(i) ((const char *) base + (i) * size)
#define CMP(i, j) cmp (ELT (i), ELT (j), data)
#define ERR2(i, j) qsort_chk_error (ELT (i), ELT (j), NULL, cmp, data)
#define ERR3(i, j, k) qsort_chk_error (ELT (i), ELT (j), ELT (k), cmp, data)
  size_t i1, i2, i, j;
  /* This outer loop iterates over maximum spans [i1, i2) such that
     elements within each span compare equal to each other.  */
  for (i1 = 0; i1 < n; i1 = i2)
    {
      /* Position i2 one past last element that compares equal to i1'th.  */
      for (i2 = i1 + 1; i2 < n; i2++)
	if (CMP (i1, i2))
	  break;
	else if (CMP (i2, i1))
	  ERR2 (i1, i2);
      size_t lim1 = LIM (i2 - i1), lim2 = LIM (n - i2);
      /* Verify that other pairs within current span compare equal.  */
      for (i = i1 + 1; i + 1 < i2; i++)
	for (j = i + 1; j < i1 + lim1; j++)
	  if (CMP (i, j))
	    ERR3 (i, i1, j);
	  else if (CMP (j, i))
	    ERR2 (i, j);
      /* Verify that elements within this span compare less than
         elements beyond the span.  */
      for (i = i1; i < i2; i++)
	for (j = i2; j < i2 + lim2; j++)
	  if (CMP (i, j) >= 0)
	    ERR3 (i, i1, j);
	  else if (CMP (j, i) <= 0)
	    ERR2 (i, j);
    }
#undef ERR3
#undef ERR2
#undef CMP
#undef ELT
#undef LIM
}
#endif /* #if CHECKING_P */

#ifndef GENERATOR_FILE
#if CHECKING_P

namespace selftest {

/* Selftests.  */

/* Call V.safe_push for all ints from START up to, but not including LIMIT.
   Helper function for selftests.  */

static void
safe_push_range (vec <int>&v, int start, int limit)
{
  for (int i = start; i < limit; i++)
    v.safe_push (i);
}

/* Verify forms of initialization.  */

static void
test_init ()
{
  {
    vec<int> v1{ };
    ASSERT_EQ (0, v1.length ());

    vec<int> v2 (v1);
    ASSERT_EQ (0, v2.length ());
  }

  {
    vec<int> v1 = vec<int>();
    ASSERT_EQ (0, v1.length ());

    vec<int> v2 = v1;
    ASSERT_EQ (0, v2.length ());
  }

  {
    vec<int> v1 (vNULL);
    ASSERT_EQ (0, v1.length ());
    v1.safe_push (1);

    vec<int> v2 (v1);
    ASSERT_EQ (1, v1.length ());
    v2.safe_push (1);

    ASSERT_EQ (2, v1.length ());
    ASSERT_EQ (2, v2.length ());
    v1.release ();
  }
}

/* Verify that vec::quick_push works correctly.  */

static void
test_quick_push ()
{
  auto_vec <int> v;
  ASSERT_EQ (0, v.length ());
  v.reserve (3);
  ASSERT_EQ (0, v.length ());
  ASSERT_TRUE (v.space (3));
  v.quick_push (5);
  v.quick_push (6);
  v.quick_push (7);
  ASSERT_EQ (3, v.length ());
  ASSERT_EQ (5, v[0]);
  ASSERT_EQ (6, v[1]);
  ASSERT_EQ (7, v[2]);
}

/* Verify that vec::safe_push works correctly.  */

static void
test_safe_push ()
{
  auto_vec <int> v;
  ASSERT_EQ (0, v.length ());
  v.safe_push (5);
  v.safe_push (6);
  v.safe_push (7);
  ASSERT_EQ (3, v.length ());
  ASSERT_EQ (5, v[0]);
  ASSERT_EQ (6, v[1]);
  ASSERT_EQ (7, v[2]);
}

/* Verify that vec::truncate works correctly.  */

static void
test_truncate ()
{
  auto_vec <int> v;
  ASSERT_EQ (0, v.length ());
  safe_push_range (v, 0, 10);
  ASSERT_EQ (10, v.length ());

  v.truncate (5);
  ASSERT_EQ (5, v.length ());
}

/* Verify that vec::safe_grow_cleared works correctly.  */

static void
test_safe_grow_cleared ()
{
  auto_vec <int> v;
  ASSERT_EQ (0, v.length ());
  v.safe_grow_cleared (50, true);
  ASSERT_EQ (50, v.length ());
  ASSERT_EQ (0, v[0]);
  ASSERT_EQ (0, v[49]);
}

/* Verify that vec::pop works correctly.  */

static void
test_pop ()
{
  auto_vec <int> v;
  safe_push_range (v, 5, 20);
  ASSERT_EQ (15, v.length ());

  int last = v.pop ();
  ASSERT_EQ (19, last);
  ASSERT_EQ (14, v.length ());
}

/* Verify that vec::safe_insert works correctly.  */

static void
test_safe_insert ()
{
  auto_vec <int> v;
  safe_push_range (v, 0, 10);
  v.safe_insert (5, 42);
  ASSERT_EQ (4, v[4]);
  ASSERT_EQ (42, v[5]);
  ASSERT_EQ (5, v[6]);
  ASSERT_EQ (11, v.length ());
}

/* Verify that vec::ordered_remove works correctly.  */

static void
test_ordered_remove ()
{
  auto_vec <int> v;
  safe_push_range (v, 0, 10);
  v.ordered_remove (5);
  ASSERT_EQ (4, v[4]);
  ASSERT_EQ (6, v[5]);
  ASSERT_EQ (9, v.length ());
}

/* Verify that vec::ordered_remove_if works correctly.  */

static void
test_ordered_remove_if (void)
{
  auto_vec <int> v;
  safe_push_range (v, 0, 10);
  unsigned ix, ix2;
  int *elem_ptr;
  VEC_ORDERED_REMOVE_IF (v, ix, ix2, elem_ptr,
			 *elem_ptr == 5 || *elem_ptr == 7);
  ASSERT_EQ (4, v[4]);
  ASSERT_EQ (6, v[5]);
  ASSERT_EQ (8, v[6]);
  ASSERT_EQ (8, v.length ());

  v.truncate (0);
  safe_push_range (v, 0, 10);
  VEC_ORDERED_REMOVE_IF_FROM_TO (v, ix, ix2, elem_ptr, 0, 6,
				 *elem_ptr == 5 || *elem_ptr == 7);
  ASSERT_EQ (4, v[4]);
  ASSERT_EQ (6, v[5]);
  ASSERT_EQ (7, v[6]);
  ASSERT_EQ (9, v.length ());

  v.truncate (0);
  safe_push_range (v, 0, 10);
  VEC_ORDERED_REMOVE_IF_FROM_TO (v, ix, ix2, elem_ptr, 0, 5,
				 *elem_ptr == 5 || *elem_ptr == 7);
  VEC_ORDERED_REMOVE_IF_FROM_TO (v, ix, ix2, elem_ptr, 8, 10,
				 *elem_ptr == 5 || *elem_ptr == 7);
  ASSERT_EQ (4, v[4]);
  ASSERT_EQ (5, v[5]);
  ASSERT_EQ (6, v[6]);
  ASSERT_EQ (10, v.length ());

  v.truncate (0);
  safe_push_range (v, 0, 10);
  VEC_ORDERED_REMOVE_IF (v, ix, ix2, elem_ptr, *elem_ptr == 5);
  ASSERT_EQ (4, v[4]);
  ASSERT_EQ (6, v[5]);
  ASSERT_EQ (7, v[6]);
  ASSERT_EQ (9, v.length ());
}

/* Verify that vec::unordered_remove works correctly.  */

static void
test_unordered_remove ()
{
  auto_vec <int> v;
  safe_push_range (v, 0, 10);
  v.unordered_remove (5);
  ASSERT_EQ (9, v.length ());
}

/* Verify that vec::block_remove works correctly.  */

static void
test_block_remove ()
{
  auto_vec <int> v;
  safe_push_range (v, 0, 10);
  v.block_remove (5, 3);
  ASSERT_EQ (3, v[3]);
  ASSERT_EQ (4, v[4]);
  ASSERT_EQ (8, v[5]);
  ASSERT_EQ (9, v[6]);
  ASSERT_EQ (7, v.length ());
}

/* Comparator for use by test_qsort.  */

static int
reverse_cmp (const void *p_i, const void *p_j)
{
  return *(const int *)p_j - *(const int *)p_i;
}

/* Verify that vec::qsort works correctly.  */

static void
test_qsort ()
{
  auto_vec <int> v;
  safe_push_range (v, 0, 10);
  v.qsort (reverse_cmp);
  ASSERT_EQ (9, v[0]);
  ASSERT_EQ (8, v[1]);
  ASSERT_EQ (1, v[8]);
  ASSERT_EQ (0, v[9]);
  ASSERT_EQ (10, v.length ());
}

/* Verify that vec::reverse works correctly.  */

static void
test_reverse ()
{
  /* Reversing an empty vec ought to be a no-op.  */
  {
    auto_vec <int> v;
    ASSERT_EQ (0, v.length ());
    v.reverse ();
    ASSERT_EQ (0, v.length ());
  }

  /* Verify reversing a vec with even length.  */
  {
    auto_vec <int> v;
    safe_push_range (v, 0, 4);
    v.reverse ();
    ASSERT_EQ (3, v[0]);
    ASSERT_EQ (2, v[1]);
    ASSERT_EQ (1, v[2]);
    ASSERT_EQ (0, v[3]);
    ASSERT_EQ (4, v.length ());
  }

  /* Verify reversing a vec with odd length.  */
  {
    auto_vec <int> v;
    safe_push_range (v, 0, 3);
    v.reverse ();
    ASSERT_EQ (2, v[0]);
    ASSERT_EQ (1, v[1]);
    ASSERT_EQ (0, v[2]);
    ASSERT_EQ (3, v.length ());
  }
}

/* A test class that increments a counter every time its dtor is called.  */

class count_dtor
{
 public:
  count_dtor (int *counter) : m_counter (counter) {}
  ~count_dtor () { (*m_counter)++; }

 private:
  int *m_counter;
};

/* Verify that auto_delete_vec deletes the elements within it.  */

static void
test_auto_delete_vec ()
{
  int dtor_count = 0;
  {
    auto_delete_vec <count_dtor> v;
    v.safe_push (new count_dtor (&dtor_count));
    v.safe_push (new count_dtor (&dtor_count));
  }
  ASSERT_EQ (dtor_count, 2);
}

/* Verify accesses to vector elements are done indirectly.  */

static void
test_auto_alias ()
{
  volatile int i = 1;
  auto_vec<int, 8> v;
  v.quick_grow (2);
  v[0] = 1;
  v[1] = 2;
  int val;
  for (int ix = i; v.iterate (ix, &val); ix++)
    ASSERT_EQ (val, 2);
  ASSERT_EQ (val, 0);
}

/* Run all of the selftests within this file.  */

void
vec_cc_tests ()
{
  test_init ();
  test_quick_push ();
  test_safe_push ();
  test_truncate ();
  test_safe_grow_cleared ();
  test_pop ();
  test_safe_insert ();
  test_ordered_remove ();
  test_ordered_remove_if ();
  test_unordered_remove ();
  test_block_remove ();
  test_qsort ();
  test_reverse ();
  test_auto_delete_vec ();
  test_auto_alias ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
#endif /* #ifndef GENERATOR_FILE */
