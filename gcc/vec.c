/* Vector API for GNU compiler.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

/* vNULL is an empty type with a template cast operation that returns
   a zero-initialized vec<T, A, L> instance.  Use this when you want
   to assign nil values to new vec instances or pass a nil vector as
   a function call argument.

   We use this technique because vec<T, A, L> must be PODs (they are
   stored in unions and passed in vararg functions), this means that
   they cannot have ctors/dtors.  */
vnull vNULL;

/* Vector memory usage.  */
struct vec_usage: public mem_usage
{
  /* Default constructor.  */
  vec_usage (): m_items (0), m_items_peak (0) {}

  /* Constructor.  */
  vec_usage (size_t allocated, size_t times, size_t peak,
	     size_t items, size_t items_peak)
    : mem_usage (allocated, times, peak),
    m_items (items), m_items_peak (items_peak) {}

  /* Comparison operator.  */
  inline bool
  operator< (const vec_usage &second) const
  {
    return (m_allocated == second.m_allocated ?
	    (m_peak == second.m_peak ? m_times < second.m_times
	     : m_peak < second.m_peak) : m_allocated < second.m_allocated);
  }

  /* Sum the usage with SECOND usage.  */
  vec_usage
  operator+ (const vec_usage &second)
  {
    return vec_usage (m_allocated + second.m_allocated,
		      m_times + second.m_times,
		      m_peak + second.m_peak,
		      m_items + second.m_items,
		      m_items_peak + second.m_items_peak);
  }

  /* Dump usage coupled to LOC location, where TOTAL is sum of all rows.  */
  inline void
  dump (mem_location *loc, mem_usage &total) const
  {
    char s[4096];
    sprintf (s, "%s:%i (%s)", loc->get_trimmed_filename (),
	     loc->m_line, loc->m_function);

    s[48] = '\0';

    fprintf (stderr, "%-48s %10li:%4.1f%%%10li%10li:%4.1f%%%11li%11li\n", s,
	     (long)m_allocated, m_allocated * 100.0 / total.m_allocated,
	     (long)m_peak, (long)m_times, m_times * 100.0 / total.m_times,
	     (long)m_items, (long)m_items_peak);
  }

  /* Dump footer.  */
  inline void
  dump_footer ()
  {
    print_dash_line ();
    fprintf (stderr, "%s%55li%25li%17li\n", "Total", (long)m_allocated,
	     (long)m_times, (long)m_items);
    print_dash_line ();
  }

  /* Dump header with NAME.  */
  static inline void
  dump_header (const char *name)
  {
    fprintf (stderr, "%-48s %11s%15s%10s%17s%11s\n", name, "Leak", "Peak",
	     "Times", "Leak items", "Peak items");
    print_dash_line ();
  }

  /* Compare wrapper used by qsort method.  */
  static int
  compare (const void *first, const void *second)
  {
    typedef std::pair<mem_location *, vec_usage *> mem_pair_t;

    const mem_pair_t f = *(const mem_pair_t *)first;
    const mem_pair_t s = *(const mem_pair_t *)second;

    return (*f.second) < (*s.second);
  }

  /* Current number of items allocated.  */
  size_t m_items;
  /* Peak value of number of allocated items.  */
  size_t m_items_peak;
};

/* Vector memory description.  */
static mem_alloc_description <vec_usage> vec_mem_desc;

/* Account the overhead.  */

void
vec_prefix::register_overhead (void *ptr, size_t size, size_t elements
			       MEM_STAT_DECL)
{
  vec_mem_desc.register_descriptor (ptr, VEC_ORIGIN, false
				    FINAL_PASS_MEM_STAT);
  vec_usage *usage = vec_mem_desc.register_instance_overhead (size, ptr);
  usage->m_items += elements;
  if (usage->m_items_peak < usage->m_items)
    usage->m_items_peak = usage->m_items;
}

/* Notice that the memory allocated for the vector has been freed.  */

void
vec_prefix::release_overhead (void *ptr, size_t size, bool in_dtor
			      MEM_STAT_DECL)
{
  if (!vec_mem_desc.contains_descriptor_for_instance (ptr))
    vec_mem_desc.register_descriptor (ptr, VEC_ORIGIN,
				      false FINAL_PASS_MEM_STAT);
  vec_mem_desc.release_instance_overhead (ptr, size, in_dtor);
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
  v.safe_grow_cleared (50);
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

/* Run all of the selftests within this file.  */

void
vec_c_tests ()
{
  test_quick_push ();
  test_safe_push ();
  test_truncate ();
  test_safe_grow_cleared ();
  test_pop ();
  test_safe_insert ();
  test_ordered_remove ();
  test_unordered_remove ();
  test_block_remove ();
  test_qsort ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
#endif /* #ifndef GENERATOR_FILE */
