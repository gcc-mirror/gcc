/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2016 Free Software Foundation, Inc.

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
#if !defined(inhibit_libc)

#ifdef L_gcov_interval_profiler
/* If VALUE is in interval <START, START + STEPS - 1>, then increases the
   corresponding counter in COUNTERS.  If the VALUE is above or below
   the interval, COUNTERS[STEPS] or COUNTERS[STEPS + 1] is increased
   instead.  */

void
__gcov_interval_profiler (gcov_type *counters, gcov_type value,
                          int start, unsigned steps)
{
  gcov_type delta = value - start;
  if (delta < 0)
    counters[steps + 1]++;
  else if (delta >= steps)
    counters[steps]++;
  else
    counters[delta]++;
}
#endif

#ifdef L_gcov_pow2_profiler
/* If VALUE is a power of two, COUNTERS[1] is incremented.  Otherwise
   COUNTERS[0] is incremented.  */

void
__gcov_pow2_profiler (gcov_type *counters, gcov_type value)
{
  if (value == 0 || (value & (value - 1)))
    counters[0]++;
  else
    counters[1]++;
}
#endif

/* Tries to determine the most common value among its inputs.  Checks if the
   value stored in COUNTERS[0] matches VALUE.  If this is the case, COUNTERS[1]
   is incremented.  If this is not the case and COUNTERS[1] is not zero,
   COUNTERS[1] is decremented.  Otherwise COUNTERS[1] is set to one and
   VALUE is stored to COUNTERS[0].  This algorithm guarantees that if this
   function is called more than 50% of the time with one value, this value
   will be in COUNTERS[0] in the end.

   In any case, COUNTERS[2] is incremented.  */

static inline void
__gcov_one_value_profiler_body (gcov_type *counters, gcov_type value)
{
  if (value == counters[0])
    counters[1]++;
  else if (counters[1] == 0)
    {
      counters[1] = 1;
      counters[0] = value;
    }
  else
    counters[1]--;
  counters[2]++;
}

#ifdef L_gcov_one_value_profiler
void
__gcov_one_value_profiler (gcov_type *counters, gcov_type value)
{
  __gcov_one_value_profiler_body (counters, value);
}
#endif

#ifdef L_gcov_indirect_call_topn_profiler
/* Tries to keep track the most frequent N values in the counters where
   N is specified by parameter TOPN_VAL. To track top N values, 2*N counter
   entries are used.
   counter[0] --- the accumative count of the number of times one entry in
                  in the counters gets evicted/replaced due to limited capacity.
                  When this value reaches a threshold, the bottom N values are
                  cleared.
   counter[1] through counter[2*N] records the top 2*N values collected so far.
   Each value is represented by two entries: count[2*i+1] is the ith value, and
   count[2*i+2] is the number of times the value is seen.  */

static void
__gcov_topn_value_profiler_body (gcov_type *counters, gcov_type value)
{
   unsigned i, found = 0, have_zero_count = 0;
   gcov_type *entry;
   gcov_type *lfu_entry = &counters[1];
   gcov_type *value_array = &counters[1];
   gcov_type *num_eviction = &counters[0];
   gcov_unsigned_t topn_val = GCOV_ICALL_TOPN_VAL;

   /* There are 2*topn_val values tracked, each value takes two slots in the
      counter array.  */
   for (i = 0; i < (topn_val << 2); i += 2)
     {
       entry = &value_array[i];
       if (entry[0] == value)
         {
           entry[1]++ ;
           found = 1;
           break;
         }
       else if (entry[1] == 0)
         {
           lfu_entry = entry;
           have_zero_count = 1;
         }
      else if (entry[1] < lfu_entry[1])
        lfu_entry = entry;
     }

   if (found)
     return;

   /* lfu_entry is either an empty entry or an entry
      with lowest count, which will be evicted.  */
   lfu_entry[0] = value;
   lfu_entry[1] = 1;

#define GCOV_ICALL_COUNTER_CLEAR_THRESHOLD 3000

   /* Too many evictions -- time to clear bottom entries to
      avoid hot values bumping each other out.  */
   if (!have_zero_count
       && ++*num_eviction >= GCOV_ICALL_COUNTER_CLEAR_THRESHOLD)
     {
       unsigned i, j;
       gcov_type *p, minv;
       gcov_type* tmp_cnts
           = (gcov_type *)alloca (topn_val * sizeof (gcov_type));

       *num_eviction = 0;

       for (i = 0; i < topn_val; i++)
         tmp_cnts[i] = 0;

       /* Find the largest topn_val values from the group of
          2*topn_val values and put them into tmp_cnts.  */

       for (i = 0; i < 2 * topn_val; i += 2)
         {
           p = 0;
           for (j = 0; j < topn_val; j++)
             {
               if (!p || tmp_cnts[j] < *p)
                  p = &tmp_cnts[j];
             }
            if (value_array[i + 1] > *p)
              *p = value_array[i + 1];
         }

       minv = tmp_cnts[0];
       for (j = 1; j < topn_val; j++)
         {
           if (tmp_cnts[j] < minv)
             minv = tmp_cnts[j];
         }
       /* Zero out low value entries.  */
       for (i = 0; i < 2 * topn_val; i += 2)
         {
           if (value_array[i + 1] < minv)
             {
               value_array[i] = 0;
               value_array[i + 1] = 0;
             }
         }
     }
}

/* These two variables are used to actually track caller and callee.  Keep
   them in TLS memory so races are not common (they are written to often).
   The variables are set directly by GCC instrumented code, so declaration
   here must match one in tree-profile.c.  */

#if defined(HAVE_CC_TLS) && !defined (USE_EMUTLS)
__thread
#endif
gcov_type *__gcov_indirect_call_topn_counters ATTRIBUTE_HIDDEN;

#if defined(HAVE_CC_TLS) && !defined (USE_EMUTLS)
__thread
#endif
void *__gcov_indirect_call_topn_callee ATTRIBUTE_HIDDEN;

#ifdef TARGET_VTABLE_USES_DESCRIPTORS
#define VTABLE_USES_DESCRIPTORS 1
#else
#define VTABLE_USES_DESCRIPTORS 0
#endif

/* This fucntion is instrumented at function entry to track topn indirect
   calls to CUR_FUNC.  */
 
void
__gcov_indirect_call_topn_profiler (gcov_type value, void* cur_func)
{
  void *callee_func = __gcov_indirect_call_topn_callee;
  /* If the C++ virtual tables contain function descriptors then one
     function may have multiple descriptors and we need to dereference
     the descriptors to see if they point to the same function.  */
  if (cur_func == callee_func
      || (VTABLE_USES_DESCRIPTORS && callee_func
	  && *(void **) cur_func == *(void **) callee_func))
    __gcov_topn_value_profiler_body (__gcov_indirect_call_topn_counters, value);
}
#endif

#ifdef L_gcov_indirect_call_profiler_v2

/* These two variables are used to actually track caller and callee.  Keep
   them in TLS memory so races are not common (they are written to often).
   The variables are set directly by GCC instrumented code, so declaration
   here must match one in tree-profile.c  */

#if defined(HAVE_CC_TLS) && !defined (USE_EMUTLS)
__thread
#endif
void * __gcov_indirect_call_callee;
#if defined(HAVE_CC_TLS) && !defined (USE_EMUTLS)
__thread
#endif
gcov_type * __gcov_indirect_call_counters;

/* By default, the C++ compiler will use function addresses in the
   vtable entries.  Setting TARGET_VTABLE_USES_DESCRIPTORS to nonzero
   tells the compiler to use function descriptors instead.  The value
   of this macro says how many words wide the descriptor is (normally 2).

   It is assumed that the address of a function descriptor may be treated
   as a pointer to a function.  */

/* Tries to determine the most common value among its inputs. */
void
__gcov_indirect_call_profiler_v2 (gcov_type value, void* cur_func)
{
  /* If the C++ virtual tables contain function descriptors then one
     function may have multiple descriptors and we need to dereference
     the descriptors to see if they point to the same function.  */
  if (cur_func == __gcov_indirect_call_callee
      || (__LIBGCC_VTABLE_USES_DESCRIPTORS__ && __gcov_indirect_call_callee
          && *(void **) cur_func == *(void **) __gcov_indirect_call_callee))
    __gcov_one_value_profiler_body (__gcov_indirect_call_counters, value);
}
#endif

#ifdef L_gcov_time_profiler

/* Counter for first visit of each function.  */
static gcov_type function_counter;

/* Sets corresponding COUNTERS if there is no value.  */

void
__gcov_time_profiler (gcov_type* counters)
{
  if (!counters[0])
    counters[0] = ++function_counter;
}
#endif

#ifdef L_gcov_average_profiler
/* Increase corresponding COUNTER by VALUE.  FIXME: Perhaps we want
   to saturate up.  */

void
__gcov_average_profiler (gcov_type *counters, gcov_type value)
{
  counters[0] += value;
  counters[1] ++;
}
#endif

#ifdef L_gcov_ior_profiler
/* Bitwise-OR VALUE into COUNTER.  */

void
__gcov_ior_profiler (gcov_type *counters, gcov_type value)
{
  *counters |= value;
}
#endif

#endif /* inhibit_libc */
