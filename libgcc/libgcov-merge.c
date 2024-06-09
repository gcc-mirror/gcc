/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2024 Free Software Foundation, Inc.

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

#include "libgcov.h"

#if defined(inhibit_libc)
/* If libc and its header files are not available, provide dummy functions.  */

#ifdef L_gcov_merge_add
void __gcov_merge_add (gcov_type *counters  __attribute__ ((unused)),
                       unsigned n_counters __attribute__ ((unused))) {}
#endif

#ifdef L_gcov_merge_ior
void __gcov_merge_ior (gcov_type *counters  __attribute__ ((unused)),
		       unsigned n_counters __attribute__ ((unused))) {}
#endif

#ifdef L_gcov_merge_topn
void __gcov_merge_topn (gcov_type *counters  __attribute__ ((unused)),
			unsigned n_counters __attribute__ ((unused))) {}
#endif

#else

#ifdef L_gcov_merge_add
/* The profile merging function that just adds the counters.  It is given
   an array COUNTERS of N_COUNTERS old counters and it reads the same number
   of counters from the gcov file.  */
void
__gcov_merge_add (gcov_type *counters, unsigned n_counters)
{
  for (; n_counters; counters++, n_counters--)
    *counters += gcov_get_counter ();
}
#endif /* L_gcov_merge_add */

#ifdef L_gcov_merge_ior
/* The profile merging function that just adds the counters.  It is given
   an array COUNTERS of N_COUNTERS old counters and it reads the same number
   of counters from the gcov file.  */
void
__gcov_merge_ior (gcov_type *counters, unsigned n_counters)
{
  for (; n_counters; counters++, n_counters--)
    *counters |= gcov_get_counter_target ();
}
#endif

#ifdef L_gcov_merge_time_profile
/* Time profiles are merged so that minimum from all valid (greater than zero)
   is stored. There could be a fork that creates new counters. To have
   the profile stable, we chosen to pick the smallest function visit time.  */
void
__gcov_merge_time_profile (gcov_type *counters, unsigned n_counters)
{
  unsigned int i;
  gcov_type value;

  for (i = 0; i < n_counters; i++)
    {
      value = gcov_get_counter_target ();

      if (value && (!counters[i] || value < counters[i]))
        counters[i] = value;
    }
}
#endif /* L_gcov_merge_time_profile */

#ifdef L_gcov_merge_topn

/* The profile merging function for choosing the most common value.
   It is given an array COUNTERS of N_COUNTERS old counters and it
   reads the same number of counters from the gcov file.  The counters
   are split into pairs where the members of the tuple have
   meanings:

   -- the stored candidate on the most common value of the measured entity
   -- counter

   We use -TOTAL for situation when merging dropped some values.
   The information is used for -fprofile-reproducible flag.
   */

void
__gcov_merge_topn (gcov_type *counters, unsigned n_counters)
{
  gcc_assert (!(n_counters % GCOV_TOPN_MEM_COUNTERS));

  for (unsigned i = 0; i < (n_counters / GCOV_TOPN_MEM_COUNTERS); i++)
    {
      /* First value is number of total executions of the profiler.  */
      gcov_type all = gcov_get_counter_ignore_scaling (-1);
      gcov_type n = gcov_get_counter_ignore_scaling (-1);

      unsigned full = all < 0;
      gcov_type *total = &counters[GCOV_TOPN_MEM_COUNTERS * i];
      *total += full ? -all : all;

      for (unsigned j = 0; j < n; j++)
	{
	  gcov_type value = gcov_get_counter_target ();
	  gcov_type count = gcov_get_counter_ignore_scaling (-1);

	  // TODO: we should use atomic here
	  full |= gcov_topn_add_value (counters + GCOV_TOPN_MEM_COUNTERS * i,
				       value, count, 0, 0);
	}

      if (full)
	*total = -(*total);
    }
}
#endif /* L_gcov_merge_topn */

#endif /* inhibit_libc */
