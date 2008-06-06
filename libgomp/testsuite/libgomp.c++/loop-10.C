// { dg-do run }

#include <omp.h>

extern "C" void abort (void);

#define LLONG_MAX __LONG_LONG_MAX__
#define ULLONG_MAX (LLONG_MAX * 2ULL + 1)
#define INT_MAX __INT_MAX__

int v;

int
test1 (void)
{
  int e = 0, cnt = 0;
  long long i;
  unsigned long long j;
  char buf[6], *p;

  #pragma omp for schedule(dynamic,1) collapse(2) nowait
  for (i = LLONG_MAX - 30001; i <= LLONG_MAX - 10001; i += 10000)
    for (j = 20; j <= LLONG_MAX - 70; j += LLONG_MAX + 50ULL)
      if ((i != LLONG_MAX - 30001
	   && i != LLONG_MAX - 20001
	   && i != LLONG_MAX - 10001)
	  || j != 20)
	e = 1;
      else
	cnt++;
  if (e || cnt != 3)
    abort ();
  else
    cnt = 0;

  #pragma omp for schedule(guided,1) collapse(2) nowait
  for (i = -LLONG_MAX + 30000; i >= -LLONG_MAX + 10000; i -= 10000)
    for (j = ULLONG_MAX - 3; j >= LLONG_MAX + 70ULL; j -= LLONG_MAX + 50ULL)
      if ((i != -LLONG_MAX + 30000
	   && i != -LLONG_MAX + 20000
	   && i != -LLONG_MAX + 10000)
	  || j != ULLONG_MAX - 3)
	e = 1;
      else
	cnt++;
  if (e || cnt != 3)
    abort ();
  else
    cnt = 0;

  #pragma omp for schedule(static,1) collapse(2) nowait
  for (i = LLONG_MAX - 30001; i <= LLONG_MAX - 10001; i += 10000)
    for (j = 20; j <= LLONG_MAX - 70 + v; j += LLONG_MAX + 50ULL)
      if ((i != LLONG_MAX - 30001
	   && i != LLONG_MAX - 20001
	   && i != LLONG_MAX - 10001)
	  || j != 20)
	e = 1;
      else
	cnt++;
  if (e || cnt != 3)
    abort ();
  else
    cnt = 0;

  #pragma omp for schedule(static) collapse(2) nowait
  for (i = -LLONG_MAX + 30000 + v; i >= -LLONG_MAX + 10000; i -= 10000)
    for (j = ULLONG_MAX - 3; j >= LLONG_MAX + 70ULL; j -= LLONG_MAX + 50ULL)
      if ((i != -LLONG_MAX + 30000
	   && i != -LLONG_MAX + 20000
	   && i != -LLONG_MAX + 10000)
	  || j != ULLONG_MAX - 3)
	e = 1;
      else
	cnt++;
  if (e || cnt != 3)
    abort ();
  else
    cnt = 0;

  #pragma omp for schedule(runtime) collapse(2) nowait
  for (i = 10; i < 30; i++)
    for (p = buf; p <= buf + 4; p += 2)
      if (i < 10 || i >= 30 || (p != buf && p != buf + 2 && p != buf + 4))
	e = 1;
      else
	cnt++;
  if (e || cnt != 60)
    abort ();
  else
    cnt = 0;

  return 0;
}

int
main (void)
{
  if (2 * sizeof (int) != sizeof (long long))
    return 0;
  asm volatile ("" : "+r" (v));
  omp_set_schedule (omp_sched_dynamic, 1);
  test1 ();
  return 0;
}
