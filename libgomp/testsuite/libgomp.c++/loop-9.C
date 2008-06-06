// { dg-do run }

#include <omp.h>

extern "C" void abort ();

#define LLONG_MAX __LONG_LONG_MAX__
#define ULLONG_MAX (LLONG_MAX * 2ULL + 1)
#define INT_MAX __INT_MAX__

int arr[6 * 5];

void
set (int loopidx, int idx)
{
#pragma omp atomic
  arr[loopidx * 5 + idx]++;
}

#define check(var, val, loopidx, idx) \
  if (var == (val)) set (loopidx, idx); else
#define test(loopidx, count) \
  for (idx = 0; idx < 5; idx++) \
    if (arr[loopidx * 5 + idx] != idx < count) \
      abort (); \
    else \
      arr[loopidx * 5 + idx] = 0

int
test1 ()
{
  int e = 0, idx;

#pragma omp parallel reduction(+:e)
  {
    long long i;
    unsigned long long j;
    #pragma omp for schedule(dynamic,1) nowait
    for (i = LLONG_MAX - 30001; i <= LLONG_MAX - 10001; i += 10000)
      {
	check (i, LLONG_MAX - 30001, 0, 0)
	check (i, LLONG_MAX - 20001, 0, 1)
	check (i, LLONG_MAX - 10001, 0, 2)
	e = 1;
      }
    #pragma omp for schedule(dynamic,1) nowait
    for (i = -LLONG_MAX + 30000; i >= -LLONG_MAX + 10000; i -= 10000)
      {
	check (i, -LLONG_MAX + 30000, 1, 0)
	check (i, -LLONG_MAX + 20000, 1, 1)
	check (i, -LLONG_MAX + 10000, 1, 2)
	e = 1;
      }
    #pragma omp for schedule(dynamic,1) nowait
    for (j = 20; j <= LLONG_MAX - 70; j += LLONG_MAX + 50ULL)
      {
	check (j, 20, 2, 0)
	e = 1;
      }
    #pragma omp for schedule(dynamic,1) nowait
    for (j = ULLONG_MAX - 3; j >= LLONG_MAX + 70ULL; j -= LLONG_MAX + 50ULL)
      {
	check (j, ULLONG_MAX - 3, 3, 0)
	e = 1;
      }
    #pragma omp for schedule(dynamic,1) nowait
    for (j = LLONG_MAX - 20000ULL; j <= LLONG_MAX + 10000ULL; j += 10000ULL)
      {
	check (j, LLONG_MAX - 20000ULL, 4, 0)
	check (j, LLONG_MAX - 10000ULL, 4, 1)
	check (j, LLONG_MAX, 4, 2)
	check (j, LLONG_MAX + 10000ULL, 4, 3)
	e = 1;
      }
    #pragma omp for schedule(dynamic,1) nowait
    for (i = -3LL * INT_MAX - 20000LL; i <= INT_MAX + 10000LL; i += INT_MAX + 200LL)
      {
	check (i, -3LL * INT_MAX - 20000LL, 5, 0)
	check (i, -2LL * INT_MAX - 20000LL + 200LL, 5, 1)
	check (i, -INT_MAX - 20000LL + 400LL, 5, 2)
	check (i, -20000LL + 600LL, 5, 3)
	check (i, INT_MAX - 20000LL + 800LL, 5, 4)
	e = 1;
      }
  }
  if (e)
    abort ();
  test (0, 3);
  test (1, 3);
  test (2, 1);
  test (3, 1);
  test (4, 4);
  test (5, 5);
  return 0;
}

int
test2 ()
{
  int e = 0, idx;

#pragma omp parallel reduction(+:e)
  {
    long long i;
    unsigned long long j;
    #pragma omp for schedule(guided,1) nowait
    for (i = LLONG_MAX - 30001; i <= LLONG_MAX - 10001; i += 10000)
      {
	check (i, LLONG_MAX - 30001, 0, 0)
	check (i, LLONG_MAX - 20001, 0, 1)
	check (i, LLONG_MAX - 10001, 0, 2)
	e = 1;
      }
    #pragma omp for schedule(guided,1) nowait
    for (i = -LLONG_MAX + 30000; i >= -LLONG_MAX + 10000; i -= 10000)
      {
	check (i, -LLONG_MAX + 30000, 1, 0)
	check (i, -LLONG_MAX + 20000, 1, 1)
	check (i, -LLONG_MAX + 10000, 1, 2)
	e = 1;
      }
    #pragma omp for schedule(guided,1) nowait
    for (j = 20; j <= LLONG_MAX - 70; j += LLONG_MAX + 50ULL)
      {
	check (j, 20, 2, 0)
	e = 1;
      }
    #pragma omp for schedule(guided,1) nowait
    for (j = ULLONG_MAX - 3; j >= LLONG_MAX + 70ULL; j -= LLONG_MAX + 50ULL)
      {
	check (j, ULLONG_MAX - 3, 3, 0)
	e = 1;
      }
    #pragma omp for schedule(guided,1) nowait
    for (j = LLONG_MAX - 20000ULL; j <= LLONG_MAX + 10000ULL; j += 10000ULL)
      {
	check (j, LLONG_MAX - 20000ULL, 4, 0)
	check (j, LLONG_MAX - 10000ULL, 4, 1)
	check (j, LLONG_MAX, 4, 2)
	check (j, LLONG_MAX + 10000ULL, 4, 3)
	e = 1;
      }
    #pragma omp for schedule(guided,1) nowait
    for (i = -3LL * INT_MAX - 20000LL; i <= INT_MAX + 10000LL; i += INT_MAX + 200LL)
      {
	check (i, -3LL * INT_MAX - 20000LL, 5, 0)
	check (i, -2LL * INT_MAX - 20000LL + 200LL, 5, 1)
	check (i, -INT_MAX - 20000LL + 400LL, 5, 2)
	check (i, -20000LL + 600LL, 5, 3)
	check (i, INT_MAX - 20000LL + 800LL, 5, 4)
	e = 1;
      }
  }
  if (e)
    abort ();
  test (0, 3);
  test (1, 3);
  test (2, 1);
  test (3, 1);
  test (4, 4);
  test (5, 5);
  return 0;
}

int
test3 ()
{
  int e = 0, idx;

#pragma omp parallel reduction(+:e)
  {
    long long i;
    unsigned long long j;
    #pragma omp for schedule(static) nowait
    for (i = LLONG_MAX - 30001; i <= LLONG_MAX - 10001; i += 10000)
      {
	check (i, LLONG_MAX - 30001, 0, 0)
	check (i, LLONG_MAX - 20001, 0, 1)
	check (i, LLONG_MAX - 10001, 0, 2)
	e = 1;
      }
    #pragma omp for schedule(static) nowait
    for (i = -LLONG_MAX + 30000; i >= -LLONG_MAX + 10000; i -= 10000)
      {
	check (i, -LLONG_MAX + 30000, 1, 0)
	check (i, -LLONG_MAX + 20000, 1, 1)
	check (i, -LLONG_MAX + 10000, 1, 2)
	e = 1;
      }
    #pragma omp for schedule(static) nowait
    for (j = 20; j <= LLONG_MAX - 70; j += LLONG_MAX + 50ULL)
      {
	check (j, 20, 2, 0)
	e = 1;
      }
    #pragma omp for schedule(static) nowait
    for (j = ULLONG_MAX - 3; j >= LLONG_MAX + 70ULL; j -= LLONG_MAX + 50ULL)
      {
	check (j, ULLONG_MAX - 3, 3, 0)
	e = 1;
      }
    #pragma omp for schedule(static) nowait
    for (j = LLONG_MAX - 20000ULL; j <= LLONG_MAX + 10000ULL; j += 10000ULL)
      {
	check (j, LLONG_MAX - 20000ULL, 4, 0)
	check (j, LLONG_MAX - 10000ULL, 4, 1)
	check (j, LLONG_MAX, 4, 2)
	check (j, LLONG_MAX + 10000ULL, 4, 3)
	e = 1;
      }
    #pragma omp for schedule(static) nowait
    for (i = -3LL * INT_MAX - 20000LL; i <= INT_MAX + 10000LL; i += INT_MAX + 200LL)
      {
	check (i, -3LL * INT_MAX - 20000LL, 5, 0)
	check (i, -2LL * INT_MAX - 20000LL + 200LL, 5, 1)
	check (i, -INT_MAX - 20000LL + 400LL, 5, 2)
	check (i, -20000LL + 600LL, 5, 3)
	check (i, INT_MAX - 20000LL + 800LL, 5, 4)
	e = 1;
      }
  }
  if (e)
    abort ();
  test (0, 3);
  test (1, 3);
  test (2, 1);
  test (3, 1);
  test (4, 4);
  test (5, 5);
  return 0;
}

int
test4 ()
{
  int e = 0, idx;

#pragma omp parallel reduction(+:e)
  {
    long long i;
    unsigned long long j;
    #pragma omp for schedule(static,1) nowait
    for (i = LLONG_MAX - 30001; i <= LLONG_MAX - 10001; i += 10000)
      {
	check (i, LLONG_MAX - 30001, 0, 0)
	check (i, LLONG_MAX - 20001, 0, 1)
	check (i, LLONG_MAX - 10001, 0, 2)
	e = 1;
      }
    #pragma omp for schedule(static,1) nowait
    for (i = -LLONG_MAX + 30000; i >= -LLONG_MAX + 10000; i -= 10000)
      {
	check (i, -LLONG_MAX + 30000, 1, 0)
	check (i, -LLONG_MAX + 20000, 1, 1)
	check (i, -LLONG_MAX + 10000, 1, 2)
	e = 1;
      }
    #pragma omp for schedule(static,1) nowait
    for (j = 20; j <= LLONG_MAX - 70; j += LLONG_MAX + 50ULL)
      {
	check (j, 20, 2, 0)
	e = 1;
      }
    #pragma omp for schedule(static,1) nowait
    for (j = ULLONG_MAX - 3; j >= LLONG_MAX + 70ULL; j -= LLONG_MAX + 50ULL)
      {
	check (j, ULLONG_MAX - 3, 3, 0)
	e = 1;
      }
    #pragma omp for schedule(static,1) nowait
    for (j = LLONG_MAX - 20000ULL; j <= LLONG_MAX + 10000ULL; j += 10000ULL)
      {
	check (j, LLONG_MAX - 20000ULL, 4, 0)
	check (j, LLONG_MAX - 10000ULL, 4, 1)
	check (j, LLONG_MAX, 4, 2)
	check (j, LLONG_MAX + 10000ULL, 4, 3)
	e = 1;
      }
    #pragma omp for schedule(static,1) nowait
    for (i = -3LL * INT_MAX - 20000LL; i <= INT_MAX + 10000LL; i += INT_MAX + 200LL)
      {
	check (i, -3LL * INT_MAX - 20000LL, 5, 0)
	check (i, -2LL * INT_MAX - 20000LL + 200LL, 5, 1)
	check (i, -INT_MAX - 20000LL + 400LL, 5, 2)
	check (i, -20000LL + 600LL, 5, 3)
	check (i, INT_MAX - 20000LL + 800LL, 5, 4)
	e = 1;
      }
  }
  if (e)
    abort ();
  test (0, 3);
  test (1, 3);
  test (2, 1);
  test (3, 1);
  test (4, 4);
  test (5, 5);
  return 0;
}

int
test5 ()
{
  int e = 0, idx;

#pragma omp parallel reduction(+:e)
  {
    long long i;
    unsigned long long j;
    #pragma omp for schedule(runtime) nowait
    for (i = LLONG_MAX - 30001; i <= LLONG_MAX - 10001; i += 10000)
      {
	check (i, LLONG_MAX - 30001, 0, 0)
	check (i, LLONG_MAX - 20001, 0, 1)
	check (i, LLONG_MAX - 10001, 0, 2)
	e = 1;
      }
    #pragma omp for schedule(runtime) nowait
    for (i = -LLONG_MAX + 30000; i >= -LLONG_MAX + 10000; i -= 10000)
      {
	check (i, -LLONG_MAX + 30000, 1, 0)
	check (i, -LLONG_MAX + 20000, 1, 1)
	check (i, -LLONG_MAX + 10000, 1, 2)
	e = 1;
      }
    #pragma omp for schedule(runtime) nowait
    for (j = 20; j <= LLONG_MAX - 70; j += LLONG_MAX + 50ULL)
      {
	check (j, 20, 2, 0)
	e = 1;
      }
    #pragma omp for schedule(runtime) nowait
    for (j = ULLONG_MAX - 3; j >= LLONG_MAX + 70ULL; j -= LLONG_MAX + 50ULL)
      {
	check (j, ULLONG_MAX - 3, 3, 0)
	e = 1;
      }
    #pragma omp for schedule(runtime) nowait
    for (j = LLONG_MAX - 20000ULL; j <= LLONG_MAX + 10000ULL; j += 10000ULL)
      {
	check (j, LLONG_MAX - 20000ULL, 4, 0)
	check (j, LLONG_MAX - 10000ULL, 4, 1)
	check (j, LLONG_MAX, 4, 2)
	check (j, LLONG_MAX + 10000ULL, 4, 3)
	e = 1;
      }
    #pragma omp for schedule(runtime) nowait
    for (i = -3LL * INT_MAX - 20000LL; i <= INT_MAX + 10000LL; i += INT_MAX + 200LL)
      {
	check (i, -3LL * INT_MAX - 20000LL, 5, 0)
	check (i, -2LL * INT_MAX - 20000LL + 200LL, 5, 1)
	check (i, -INT_MAX - 20000LL + 400LL, 5, 2)
	check (i, -20000LL + 600LL, 5, 3)
	check (i, INT_MAX - 20000LL + 800LL, 5, 4)
	e = 1;
      }
  }
  if (e)
    abort ();
  test (0, 3);
  test (1, 3);
  test (2, 1);
  test (3, 1);
  test (4, 4);
  test (5, 5);
  return 0;
}

int
main ()
{
  if (2 * sizeof (int) != sizeof (long long))
    return 0;
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  omp_set_schedule (omp_sched_static, 0);
  test5 ();
  omp_set_schedule (omp_sched_static, 3);
  test5 ();
  omp_set_schedule (omp_sched_dynamic, 5);
  test5 ();
  omp_set_schedule (omp_sched_guided, 2);
  test5 ();
  return 0;
}
