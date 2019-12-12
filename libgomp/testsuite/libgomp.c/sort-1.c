/* Test and benchmark of a couple of parallel sorting algorithms.
   Copyright (C) 2008-2019 Free Software Foundation, Inc.

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

#include <limits.h>
#include <omp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int failures;

#define THRESHOLD 100

static void
verify (const char *name, double stime, int *array, int count)
{
  int i;
  double etime = omp_get_wtime ();

  printf ("%s: %g\n", name, etime - stime);
  for (i = 1; i < count; i++)
    if (array[i] < array[i - 1])
      {
	printf ("%s: incorrectly sorted\n", name);
	failures = 1;
      }
}

static void
insertsort (int *array, int s, int e)
{
  int i, j, val;
  for (i = s + 1; i <= e; i++)
    {
      val = array[i];
      j = i;
      while (j-- > s && val < array[j])
	array[j + 1] = array[j];
      array[j + 1] = val;
    }
}

struct int_pair
{
  int lo;
  int hi;
};

struct int_pair_stack
{
  struct int_pair *top;
#define STACK_SIZE 4 * CHAR_BIT * sizeof (int)
  struct int_pair arr[STACK_SIZE];
};

static inline void
init_int_pair_stack (struct int_pair_stack *stack)
{
  stack->top = &stack->arr[0];
}

static inline void
push_int_pair_stack (struct int_pair_stack *stack, int lo, int hi)
{
  stack->top->lo = lo;
  stack->top->hi = hi;
  stack->top++;
}

static inline void
pop_int_pair_stack (struct int_pair_stack *stack, int *lo, int *hi)
{
  stack->top--;
  *lo = stack->top->lo;
  *hi = stack->top->hi;
}

static inline int
size_int_pair_stack (struct int_pair_stack *stack)
{
  return stack->top - &stack->arr[0];
}

static inline void
busy_wait (void)
{
#if defined __i386__ || defined __x86_64__
  __builtin_ia32_pause ();
#elif defined __ia64__
  __asm volatile ("hint @pause" : : : "memory");
#elif defined __sparc__ && (defined __arch64__ || defined __sparc_v9__)
  __asm volatile ("membar #LoadLoad" : : : "memory");
#else
  __asm volatile ("" : : : "memory");
#endif
}

static inline void
swap (int *array, int a, int b)
{
  int val = array[a];
  array[a] = array[b];
  array[b] = val;
}

static inline int
choose_pivot (int *array, int lo, int hi)
{
  int mid = (lo + hi) / 2;

  if (array[mid] < array[lo])
    swap (array, lo, mid);
  if (array[hi] < array[mid])
    {
      swap (array, mid, hi);
      if (array[mid] < array[lo])
	swap (array, lo, mid);
    }
  return array[mid];
}

static inline int
partition (int *array, int lo, int hi)
{
  int pivot = choose_pivot (array, lo, hi);
  int left = lo;
  int right = hi;

  for (;;)
    {
      while (array[++left] < pivot);
      while (array[--right] > pivot);
      if (left >= right)
	break;
      swap (array, left, right);
    }
  return left;
}

static void
sort1 (int *array, int count)
{
  omp_lock_t lock;
  struct int_pair_stack global_stack;
  int busy = 1;
  int num_threads;

  omp_init_lock (&lock);
  init_int_pair_stack (&global_stack);
  #pragma omp parallel firstprivate (array, count)
  {
    int lo = 0, hi = 0, mid, next_lo, next_hi;
    bool idle = true;
    struct int_pair_stack local_stack;

    init_int_pair_stack (&local_stack);
    if (omp_get_thread_num () == 0)
      {
	num_threads = omp_get_num_threads ();
	hi = count - 1;
	idle = false;
      }

    for (;;)
      {
	if (hi - lo < THRESHOLD)
	  {
	    insertsort (array, lo, hi);
	    lo = hi;
	  }
	if (lo >= hi)
	  {
	    if (size_int_pair_stack (&local_stack) == 0)
	      {
	      again:
		omp_set_lock (&lock);
		if (size_int_pair_stack (&global_stack) == 0)
		  {
		    if (!idle)
		      busy--;
		    if (busy == 0)
		      {
			omp_unset_lock (&lock);
			break;
		      }
		    omp_unset_lock (&lock);
		    idle = true;
		    while (size_int_pair_stack (&global_stack) == 0
			   && busy)
		      busy_wait ();
		    goto again;
		  }
		if (idle)
		  busy++;
		pop_int_pair_stack (&global_stack, &lo, &hi);
		omp_unset_lock (&lock);
		idle = false;
	      }
	    else
	      pop_int_pair_stack (&local_stack, &lo, &hi);
	  }

	mid = partition (array, lo, hi);
	if (mid - lo < hi - mid)
	  {
	    next_lo = mid;
	    next_hi = hi;
	    hi = mid - 1;
	  }
	else
	  {
	    next_lo = lo;
	    next_hi = mid - 1;
	    lo = mid;
	  }

	if (next_hi - next_lo < THRESHOLD)
	  insertsort (array, next_lo, next_hi);
	else
	  {
	    if (size_int_pair_stack (&global_stack) < num_threads - 1)
	      {
		int size;

		omp_set_lock (&lock);
		size = size_int_pair_stack (&global_stack);
		if (size < num_threads - 1 && size < STACK_SIZE)
		  push_int_pair_stack (&global_stack, next_lo, next_hi);
		else
		  push_int_pair_stack (&local_stack, next_lo, next_hi);
		omp_unset_lock (&lock);
	      }
	    else
	      push_int_pair_stack (&local_stack, next_lo, next_hi);
	  }
      }
    }
  omp_destroy_lock (&lock);
}

static void
sort2_1 (int *array, int lo, int hi, int num_threads, int *busy)
{
  int mid;

  if (hi - lo < THRESHOLD)
    {
      insertsort (array, lo, hi);
      return;
    }

  mid = partition (array, lo, hi);

  if (*busy >= num_threads)
    {
      sort2_1 (array, lo, mid - 1, num_threads, busy);
      sort2_1 (array, mid, hi, num_threads, busy);
      return;
    }

  #pragma omp atomic
    *busy += 1;

  #pragma omp parallel num_threads (2) \
		       firstprivate (array, lo, hi, mid, num_threads, busy)
  {
    if (omp_get_thread_num () == 0)
      sort2_1 (array, lo, mid - 1, num_threads, busy);
    else
      {
	sort2_1 (array, mid, hi, num_threads, busy);
	#pragma omp atomic
	  *busy -= 1;
      }
  }
}

static void
sort2 (int *array, int count)
{
  int num_threads;
  int busy = 1;

  #pragma omp parallel
    #pragma omp single nowait
      num_threads = omp_get_num_threads ();

  sort2_1 (array, 0, count - 1, num_threads, &busy);
}

#if _OPENMP >= 200805
static void
sort3_1 (int *array, int lo, int hi)
{
  int mid;

  if (hi - lo < THRESHOLD)
    {
      insertsort (array, lo, hi);
      return;
    }

  mid = partition (array, lo, hi);
  #pragma omp task
    sort3_1 (array, lo, mid - 1);
  sort3_1 (array, mid, hi);
}

static void
sort3 (int *array, int count)
{
  #pragma omp parallel
    #pragma omp single
      sort3_1 (array, 0, count - 1);
}
#endif

int
main (int argc, char **argv)
{
  int i, count = 1000000;
  double stime;
  int *unsorted, *sorted, num_threads;
  if (argc >= 2)
    count = strtoul (argv[1], NULL, 0);

  unsorted = malloc (count * sizeof (int));
  sorted = malloc (count * sizeof (int));
  if (unsorted == NULL || sorted == NULL)
    {
      puts ("allocation failure");
      exit (1);
    }

  srand (0xdeadbeef);
  for (i = 0; i < count; i++)
    unsorted[i] = rand ();

  omp_set_nested (1);
  omp_set_dynamic (0);
  #pragma omp parallel
    #pragma omp single nowait
      num_threads = omp_get_num_threads ();
  printf ("Threads: %d\n", num_threads);

  memcpy (sorted, unsorted, count * sizeof (int));
  stime = omp_get_wtime ();
  sort1 (sorted, count);
  verify ("sort1", stime, sorted, count);

  memcpy (sorted, unsorted, count * sizeof (int));
  stime = omp_get_wtime ();
  sort2 (sorted, count);
  verify ("sort2", stime, sorted, count);

#if _OPENMP >= 200805
  memcpy (sorted, unsorted, count * sizeof (int));
  stime = omp_get_wtime ();
  sort3 (sorted, count);
  verify ("sort3", stime, sorted, count);
#endif

  return 0;
}
