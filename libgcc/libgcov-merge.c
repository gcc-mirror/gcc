/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2020 Free Software Foundation, Inc.

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

/* To merging of TOPN profiles.
   counters[0] is the number of executions
   for i in 0 ... TOPN-1
     counters[2 * i + 1] is target
     counters[2 * i + 2] is corresponding hitrate counter.

   Because we prune counters only those with probability >= 1/TOPN are
   present now.

   We use sign of counters[0] to track whether the number of different
   targets exceeds TOPN.  */

static void
merge_topn_values_set (gcov_type *counters)
{
  /* First value is number of total executions of the profiler.  */
  gcov_type all = gcov_get_counter ();
  gcov_type *total = &counters[0];
  ++counters;

  /* Negative value means that counter is missing some of values.  */
  if (all < 0)
    *total = -(*total);

  *total += all;

  /* Read all part values.  */
  gcov_type read_counters[2 * GCOV_TOPN_VALUES];
  for (unsigned i = 0; i < GCOV_TOPN_VALUES; i++)
    {
      read_counters[2 * i] = gcov_get_counter_target ();
      read_counters[2 * i + 1] = gcov_get_counter_ignore_scaling (-1);
    }

  for (unsigned i = 0; i < GCOV_TOPN_VALUES; i++)
    {
      if (read_counters[2 * i + 1] == 0)
	continue;

      unsigned j;
      int slot = 0;

      for (j = 0; j < GCOV_TOPN_VALUES; j++)
	{
	  if (counters[2 * j] == read_counters[2 * i])
	    {
	      counters[2 * j + 1] += read_counters[2 * i + 1];
	      break;
	    }
	  else if (counters[2 * j + 1] < counters[2 * slot + 1])
	    slot = j;
	}

      if (j == GCOV_TOPN_VALUES)
	{
	  gcov_type slot_count = counters[2 * slot + 1];
	  /* We found an empty slot.  */
	  if (slot_count == 0)
	    {
	      /* If we found empty slot, add the value.  */
	      counters[2 * slot] = read_counters[2 * i];
	      counters[2 * slot + 1] = read_counters[2 * i + 1];
	    }
	  else
	    {
	      /* Here we are loosing some values.  */
	      if (*total >= 0)
		*total = -(*total);
	      if (read_counters[2 * i + 1] > slot_count)
		{
		  counters[2 * slot] = read_counters[2 * i];
		  counters[2 * slot + 1] = read_counters[2 * i + 1];
		}
	      else
		counters[2 * slot + 1] -= read_counters[2 * i + 1];
	    }
	}
    }
}

/* The profile merging function for choosing the most common value.
   It is given an array COUNTERS of N_COUNTERS old counters and it
   reads the same number of counters from the gcov file.  The counters
   are split into pairs where the members of the tuple have
   meanings:

   -- the stored candidate on the most common value of the measured entity
   -- counter
   */
void
__gcov_merge_topn (gcov_type *counters, unsigned n_counters)
{
  gcc_assert (!(n_counters % GCOV_TOPN_VALUES_COUNTERS));

  for (unsigned i = 0; i < (n_counters / GCOV_TOPN_VALUES_COUNTERS); i++)
    merge_topn_values_set (counters + (i * GCOV_TOPN_VALUES_COUNTERS));
}
#endif /* L_gcov_merge_topn */

#endif /* inhibit_libc */
