#include <omp.h>
#include <stdlib.h>
#include <stdint.h>

void
foo (int x, omp_allocator_handle_t h, int fl)
{
  int y = 0, r = 0, i, i1, l, l2[4], l3, n = 8;
  int i2, j2, n2 = 9, l4;
  int i3, j3, n3 = 10, l5;
  int i4, j4, n4 = 11, l6;
  int i5;
  int v[x], w[x];
  int xo = x;
  for (i = 0; i < x; i++)
    w[i] = i;
  #pragma omp parallel private (y, v) firstprivate (x) allocate (x, y, v)
  {
    int *volatile p1 = &x;
    int *volatile p2 = &y;
    if (x != 42)
      abort ();
    #pragma omp barrier
    *p2 = 1;
    p1[0]++;
    v[0] = 7;
    v[41] = 8;
    #pragma omp barrier
    if (x != 43 || y != 1)
      abort ();
    if (v[0] != 7 || v[41] != 8)
      abort ();
    if ((fl & 2) && (((uintptr_t) p1 | (uintptr_t) p2
	| (uintptr_t) &v[0]) & 63) != 0)
      abort ();
  }
  x = xo;
  #pragma omp teams
  #pragma omp parallel private (y) firstprivate (x, w) allocate (h: x, y, w)
  {
    int *volatile p1 = &x;
    int *volatile p2 = &y;
    if (x != 42 || w[17] != 17 || w[41] != 41)
      abort ();
    #pragma omp barrier
    *p2 = 1;
    p1[0]++;
    w[19]++;
    #pragma omp barrier
    if (x != 43 || y != 1 || w[19] != 20)
      abort ();
    if ((fl & 1) && (((uintptr_t) p1 | (uintptr_t) p2
		      | (uintptr_t) &w[0]) & 63) != 0)
      abort ();
  }
  x = xo;
  #pragma omp parallel for private (y) firstprivate (x) allocate (h: x, y, r, l, n) reduction(+: r) lastprivate (l) linear (n: 16)
  for (i = 0; i < 64; i++)
    {
      if (x != 42)
	abort ();
      y = 1;
      l = i;
      n += y + 15;
      r += i;
      if ((fl & 1) && (((uintptr_t) &x | (uintptr_t) &y | (uintptr_t) &r
			| (uintptr_t) &l | (uintptr_t) &n) & 63) != 0)
	abort ();
    }
  #pragma omp parallel
  {
    #pragma omp for lastprivate (l2) private (i1) allocate (h: l2, l3, i1) lastprivate (conditional: l3)
    for (i1 = 0; i1 < 64; i1++)
      {
	l2[0] = i1;
	l2[1] = i1 + 1;
	l2[2] = i1 + 2;
	l2[3] = i1 + 3;
	if (i1 < 37)
	  l3 = i1;
	if ((fl & 1) && (((uintptr_t) &l2[0] | (uintptr_t) &l3 | (uintptr_t) &i1) & 63) != 0)
	  abort ();
      }
    #pragma omp for collapse(2) lastprivate(l4, i2, j2) linear (n2:17) allocate (h: n2, l4, i2, j2)
    for (i2 = 3; i2 < 5; i2++)
      for (j2 = 17; j2 < 22; j2 += 2)
	{
	  n2 += 17;
	  l4 = i2 * 31 + j2;
	  if ((fl & 1) && (((uintptr_t) &l4 | (uintptr_t) &n2
			    | (uintptr_t) &i2 | (uintptr_t) &j2) & 63) != 0)
	    abort ();
	}
    #pragma omp for collapse(2) lastprivate(l5, i3, j3) linear (n3:17) schedule (static, 3) allocate (n3, l5, i3, j3)
    for (i3 = 3; i3 < 5; i3++)
      for (j3 = 17; j3 < 23; j3 += 2)
	{
	  n3 += 17;
	  l5 = i3 * 31 + j3;
	  if ((fl & 2) && (((uintptr_t) &l5 | (uintptr_t) &n3
			    | (uintptr_t) &i3 | (uintptr_t) &j3) & 63) != 0)
	    abort ();
	}
    #pragma omp for collapse(2) lastprivate(l6, i4, j4) linear (n4:17) schedule (dynamic) allocate (h: n4, l6, i4, j4)
    for (i4 = 3; i4 < 5; i4++)
      for (j4 = 17; j4 < 22; j4 += 2)
	{
	  n4 += 17;
	  l6 = i4 * 31 + j4;
	  if ((fl & 1) && (((uintptr_t) &l6 | (uintptr_t) &n4
			    | (uintptr_t) &i4 | (uintptr_t) &j4) & 63) != 0)
	    abort ();
	}
    #pragma omp for lastprivate (i5) allocate (i5)
    for (i5 = 1; i5 < 17; i5 += 3)
      {
	if ((fl & 2) && (((uintptr_t) &i5) & 63) != 0)
	  abort ();
      }
  }
  if (r != 64 * 63 / 2 || l != 63 || n != 8 + 16 * 64)
    abort ();
  if (l2[0] != 63 || l2[1] != 63 + 1 || l2[2] != 63 + 2 || l2[3] != 63 + 3 || l3 != 36)
    abort ();
  if (i2 != 5 || j2 != 23 || n2 != 9 + 6 * 17 || l4 != 4 * 31 + 21)
    abort ();
  if (i3 != 5 || j3 != 23 || n3 != 10 + 6 * 17 || l5 != 4 * 31 + 21)
    abort ();
  if (i4 != 5 || j4 != 23 || n4 != 11 + 6 * 17 || l6 != 4 * 31 + 21)
    abort ();
  if (i5 != 19)
    abort ();
}

void
bar (int x, omp_allocator_handle_t h)
{
  int y = 0, r = 0, i, i1, l, l2[4], l3, n = 8;
  int i2, j2, n2 = 9, l4;
  int i3, j3, n3 = 10, l5;
  int i4, j4, n4 = 11, l6;
  int i5;
  int xo = x;
  #pragma omp parallel private (y) firstprivate (x) allocate (x, y)
  {
    int *volatile p1 = &x;
    int *volatile p2 = &y;
    if (x != 42)
      abort ();
    #pragma omp barrier
    *p2 = 1;
    p1[0]++;
    #pragma omp barrier
    if (x != 43 || y != 1)
      abort ();
  }
  x = xo;
  #pragma omp teams
  #pragma omp parallel private (y) firstprivate (x) allocate (h: x, y)
  {
    int *volatile p1 = &x;
    int *volatile p2 = &y;
    if (x != 42)
      abort ();
    #pragma omp barrier
    *p2 = 1;
    p1[0]++;
    #pragma omp barrier
    if (x != 43 || y != 1)
      abort ();
  }
  x = xo;
  #pragma omp parallel for private (y) firstprivate (x) allocate (h: x, y, r, l, n) reduction(+: r) lastprivate (l) linear (n: 16)
  for (i = 0; i < 64; i++)
    {
      if (x != 42)
	abort ();
      y = 1;
      l = i;
      n += y + 15;
      r += i;
    }
  #pragma omp parallel
  {
    #pragma omp for lastprivate (l2) private (i1) allocate (h: l2, l3, i1) lastprivate (conditional: l3)
    for (i1 = 0; i1 < 64; i1++)
      {
	l2[0] = i1;
	l2[1] = i1 + 1;
	l2[2] = i1 + 2;
	l2[3] = i1 + 3;
	if (i1 < 37)
	  l3 = i1;
      }
    #pragma omp for collapse(2) lastprivate(l4, i2, j2) linear (n2:17) allocate (h: n2, l4, i2, j2)
    for (i2 = 3; i2 < 5; i2++)
      for (j2 = 17; j2 < 22; j2 += 2)
	{
	  n2 += 17;
	  l4 = i2 * 31 + j2;
	}
    #pragma omp for collapse(2) lastprivate(l5, i3, j3) linear (n3:17) schedule (static, 3) allocate (n3, l5, i3, j3)
    for (i3 = 3; i3 < 5; i3++)
      for (j3 = 17; j3 < 23; j3 += 2)
	{
	  n3 += 17;
	  l5 = i3 * 31 + j3;
	}
    #pragma omp for collapse(2) lastprivate(l6, i4, j4) linear (n4:17) schedule (dynamic) allocate (h: n4, l6, i4, j4)
    for (i4 = 3; i4 < 5; i4++)
      for (j4 = 17; j4 < 22; j4 += 2)
	{
	  n4 += 17;
	  l6 = i4 * 31 + j4;
	}
    #pragma omp for lastprivate (i5) allocate (i5)
    for (i5 = 1; i5 < 17; i5 += 3)
      ;
  }
  if (r != 64 * 63 / 2 || l != 63 || n != 8 + 16 * 64)
    abort ();
  if (l2[0] != 63 || l2[1] != 63 + 1 || l2[2] != 63 + 2 || l2[3] != 63 + 3 || l3 != 36)
    abort ();
  if (i2 != 5 || j2 != 23 || n2 != 9 + 6 * 17 || l4 != 4 * 31 + 21)
    abort ();
  if (i3 != 5 || j3 != 23 || n3 != 10 + 6 * 17 || l5 != 4 * 31 + 21)
    abort ();
  if (i4 != 5 || j4 != 23 || n4 != 11 + 6 * 17 || l6 != 4 * 31 + 21)
    abort ();
  if (i5 != 19)
    abort ();
}

int
main ()
{
  omp_alloctrait_t traits[3]
    = { { omp_atk_alignment, 64 },
	{ omp_atk_fallback, omp_atv_null_fb } };
  omp_allocator_handle_t a
    = omp_init_allocator (omp_default_mem_space, 2, traits);
  if (a == omp_null_allocator)
    abort ();
  omp_set_default_allocator (omp_default_mem_alloc);
  foo (42, omp_null_allocator, 0);
  foo (42, omp_default_mem_alloc, 0);
  foo (42, a, 1);
  omp_set_default_allocator (a);
  foo (42, omp_null_allocator, 3);
  foo (42, omp_default_mem_alloc, 2);
  bar (42, a);
  omp_destroy_allocator (a);
  return 0;
}
