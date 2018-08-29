/* { dg-do run } */

#ifndef MONOTONIC_TYPE
#include <omp.h>
#include <stdlib.h>
#define MONOTONIC_TYPE int
#define MONOTONIC_UNDEF -1
#define MONOTONIC_END(n) n
#endif

int
main ()
{
  MONOTONIC_TYPE i;
  #pragma omp parallel
  {
    int cnt = omp_get_num_threads ();
    int thr = omp_get_thread_num ();
    MONOTONIC_TYPE l = MONOTONIC_UNDEF;
    int c = 0;
    int n = 0;
    #pragma omp for nowait schedule(static, 5)
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  {
	    if (l >= i)
	      abort ();
	    if (cnt == 1)
	      abort ();
	    if (n != 5)
	      abort ();
	    n = 1;
	    c++;
	  }
	if (n == 1)
	  {
	    if ((i % 5) != 0)
	      abort ();
	    if ((i / 5) % cnt != thr)
	      abort ();
	  }
	l = i;
      }
    if (cnt == 1)
      {
	if (n != 73 || l != 73 - 1 || c != 1)
	  abort ();
      }
    else if (thr > 73 / 5)
      {
	if (l != MONOTONIC_UNDEF || c != 0 || n != 0)
	  abort ();
      }
    else if (thr == 73 / 5)
      {
	if (l != 73 - 1 || c != 1 || n != 73 % 5)
	  abort ();
      }
    else if (c == 0)
      abort ();
    else if (l == 73 - 1)
      {
	if (thr != (73 / 5) % cnt || n != 73 % 5)
	  abort ();
      }
    else if ((n % 5) != 0)
      abort ();
    l = MONOTONIC_UNDEF;
    c = 0;
    n = 0;
    #pragma omp for schedule( monotonic: static, 7) nowait
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  {
	    if (l >= i)
	      abort ();
	    if (cnt == 1)
	      abort ();
	    if (n != 7)
	      abort ();
	    n = 1;
	    c++;
	  }
	if (n == 1)
	  {
	    if ((i % 7) != 0)
	      abort ();
	    if ((i / 7) % cnt != thr)
	      abort ();
	  }
	l = i;
      }
    if (cnt == 1)
      {
	if (n != 73 || l != 73 - 1 || c != 1)
	  abort ();
      }
    else if (thr > 73 / 7)
      {
	if (l != MONOTONIC_UNDEF || c != 0 || n != 0)
	  abort ();
      }
    else if (thr == 73 / 7)
      {
	if (l != 73 - 1 || c != 1 || n != 73 % 7)
	  abort ();
      }
    else if (c == 0)
      abort ();
    else if (l == 73 - 1)
      {
	if (thr != (73 / 7) % cnt || n != 73 % 7)
	  abort ();
      }
    else if ((n % 7) != 0)
      abort ();
    l = MONOTONIC_UNDEF;
    c = 0;
    n = 0;
    #pragma omp for nowait schedule(static)
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  abort ();
	l = i;
      }
    if (c > 1)
      abort ();
    l = MONOTONIC_UNDEF;
    c = 0;
    n = 0;
    #pragma omp for nowait schedule(monotonic,simd:static)
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  abort ();
	l = i;
      }
    if (c > 1)
      abort ();
    l = MONOTONIC_UNDEF;
    c = 0;
    n = 0;
    #pragma omp for schedule(monotonic : dynamic, 5) nowait
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  {
	    if (l >= i)
	      abort ();
	    if ((n % 5) != 0 || n == 0)
	      abort ();
	    n = 1;
	    c++;
	  }
	l = i;
      }
    if (l == 73 - 1)
      {
	if (n % 5 != 73 % 5)
	  abort ();
      }
    else if (l == MONOTONIC_UNDEF)
      {
	if (n != 0 || c != 0)
	  abort ();
      }
    else if ((n % 5) != 0 || n == 0)
      abort ();
    l = MONOTONIC_UNDEF;
    c = 0;
    n = 0;
    #pragma omp for nowait schedule(dynamic, 7) ordered(1)
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  {
	    if (l >= i)
	      abort ();
	    if ((n % 7) != 0 || n == 0)
	      abort ();
	    n = 1;
	    c++;
	  }
	#pragma omp ordered depend(source)
	if (MONOTONIC_UNDEF > 0)
	  {
	    #pragma omp ordered depend(sink: i)
	  }
	else
	  {
	    #pragma omp ordered depend(sink: i - 1)
	  }
	l = i;
      }
    if (l == 73 - 1)
      {
	if (n % 7 != 73 % 7)
	  abort ();
      }
    else if (l == MONOTONIC_UNDEF)
      {
	if (n != 0 || c != 0)
	  abort ();
      }
    else if ((n % 7) != 0 || n == 0)
      abort ();
    l = MONOTONIC_UNDEF;
    c = 0;
    n = 0;
    #pragma omp for schedule (monotonic :guided , 7) nowait
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  {
	    if (l >= i)
	      abort ();
	    if (n < 7)
	      abort ();
	    n = 1;
	    c++;
	  }
	l = i;
      }
    l = MONOTONIC_UNDEF;
    c = 0;
    n = 0;
    #pragma omp for nowait schedule(guided, 7) ordered
    for (i = 0; i < MONOTONIC_END (73); i++)
      {
	if (l == MONOTONIC_UNDEF)
	  {
	    n = 1;
	    c++;
	  }
	else if (l == i - 1)
	  n++;
	else
	  {
	    if (l >= i)
	      abort ();
	    if (n < 7)
	      abort ();
	    n = 1;
	    c++;
	  }
	#pragma omp ordered
	  l = i;
      }
  }
  return 0;
}
