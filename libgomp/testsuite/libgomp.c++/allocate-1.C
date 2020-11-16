#include <omp.h>
#include <stdlib.h>
#include <stdint.h>

struct S { int a, b; };

void
foo (int &x, int &y, int &r, int &l, int (&l2)[4], int &l3, int &n, int *&p,
     int *&q, int &px, struct S &s, omp_allocator_handle_t h, int fl)
{
  int i;
  typedef int T[x];
  T v, w;
  T &v2 = v;
  T &w2 = w;
  int r1[4] = { 0, 0, 0, 0 };
  int (&r2)[4] = r1;
  int xo = x;
  for (i = 0; i < x; i++)
    w[i] = i;
  for (i = 0; i < 4; i++)
    p[i] = 0;
  for (i = 0; i < 3; i++)
    q[i] = 0;
  #pragma omp parallel private (y, v2) firstprivate (x) allocate (x, y, v2)
  {
    int *volatile p1 = &x;
    int *volatile p2 = &y;
    if (x != 42)
      abort ();
    #pragma omp barrier
    *p2 = 1;
    p1[0]++;
    v2[0] = 7;
    v2[41] = 8;
    #pragma omp barrier
    if (x != 43 || y != 1)
      abort ();
    if (v2[0] != 7 || v2[41] != 8)
      abort ();
    if ((fl & 2) && (((uintptr_t) p1 | (uintptr_t) p2
	| (uintptr_t) &v2[0]) & 63) != 0)
      abort ();
  }
  x = xo;
  #pragma omp teams
  #pragma omp parallel private (y) firstprivate (x, w2) allocate (h: x, y, w2)
  {
    int *volatile p1 = &x;
    int *volatile p2 = &y;
    if (x != 42 || w2[17] != 17 || w2[41] != 41)
      abort ();
    #pragma omp barrier
    *p2 = 1;
    p1[0]++;
    w2[19]++;
    #pragma omp barrier
    if (x != 43 || y != 1 || w2[19] != 20)
      abort ();
    if ((fl & 1) && (((uintptr_t) p1 | (uintptr_t) p2
		      | (uintptr_t) &w2[0]) & 63) != 0)
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
  x = xo;
  #pragma omp parallel
  {
    #pragma omp for lastprivate (l2) allocate (h: l2, l3) lastprivate (conditional: l3)
    for (i = 0; i < 64; i++)
      {
	l2[0] = i;
	l2[1] = i + 1;
	l2[2] = i + 2;
	l2[3] = i + 3;
	if (i < 37)
	  l3 = i;
	if ((fl & 1) && (((uintptr_t) &l2[0] | (uintptr_t) &l3) & 63) != 0)
	  abort ();
      }
    #pragma omp for reduction(+:p[2:px], q[:3], r2) allocate(h: p, q, r2)
    for (i = 0; i < 32; i++)
      {
	p[2] += i;
	p[3] += 2 * i;
	q[0] += 3 * i;
	q[2] += 4 * i;
	r2[0] += 5 * i;
	r2[3] += 6 * i;
	/* Can't really rely on alignment of &p[0], the implementation could
	   allocate the whole array or do what GCC does and allocate only part
	   of it.  */
	if ((fl & 1) && (((uintptr_t) &q[0] | (uintptr_t) &r2[0]) & 63) != 0)
	  abort ();
      }
    #pragma omp task private(y) firstprivate(x) allocate(x, y)
    {
      int *volatile p1 = &x;
      int *volatile p2 = &y;
      if (x != 42)
	abort ();
      p1[0]++;
      p2[0] = 21;
      if (x != 43 || y != 21)
	abort ();
      if ((fl & 2) && (((uintptr_t) p1 | (uintptr_t) p2) & 63) != 0)
	abort ();
    }
    #pragma omp task private(y) firstprivate(x) allocate(h: x, y)
    {
      int *volatile p1 = &x;
      int *volatile p2 = &y;
      if (x != 42)
	abort ();
      p1[0]++;
      p2[0] = 21;
      if (x != 43 || y != 21)
	abort ();
      if ((fl & 1) && (((uintptr_t) p1 | (uintptr_t) p2) & 63) != 0)
	abort ();
    }
    #pragma omp task private(y) firstprivate(s) allocate(s, y)
    {
      int *volatile p1 = &s.a;
      int *volatile p2 = &s.b;
      int *volatile p3 = &y;
      if (s.a != 27 || s.b != 29)
	abort ();
      p1[0]++;
      p2[0]++;
      p3[0] = 21;
      if (s.a != 28 || s.b != 30 || y != 21)
	abort ();
      if ((fl & 2) && (((uintptr_t) p1 | (uintptr_t) p3) & 63) != 0)
	abort ();
    }
    #pragma omp task private(y) firstprivate(s) allocate(h: s, y)
    {
      int *volatile p1 = &s.a;
      int *volatile p2 = &s.b;
      int *volatile p3 = &y;
      if (s.a != 27 || s.b != 29)
	abort ();
      p1[0]++;
      p2[0]++;
      p3[0] = 21;
      if (s.a != 28 || s.b != 30 || y != 21)
	abort ();
      if ((fl & 1) && (((uintptr_t) p1 | (uintptr_t) p3) & 63) != 0)
	abort ();
    }
  }
  if (r != 64 * 63 / 2 || l != 63 || n != 8 + 16 * 64)
    abort ();
  if (l2[0] != 63 || l2[1] != 63 + 1 || l2[2] != 63 + 2 || l2[3] != 63 + 3 || l3 != 36)
    abort ();
  if (p[2] != (32 * 31) / 2 || p[3] != 2 * (32 * 31) / 2
      || q[0] != 3 * (32 * 31) / 2 || q[2] != 4 * (32 * 31) / 2
      || r2[0] != 5 * (32 * 31) / 2 || r2[3] != 6 * (32 * 31) / 2)
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
  struct S s = { 27, 29 };
  int p1[4], q1[3], px = 2;
  int *p = p1;
  int *q = q1;
  int x = 42, y = 0, r = 0, l, l2[4], l3, n = 8;
  foo (x, y, r, l, l2, l3, n, p, q, px, s, omp_null_allocator, 0);
  x = 42; y = 0; r = 0; l = -1; l2[0] = -1; l2[1] = -1;
  l2[2] = -1; l2[3] = -1; n = 8;
  foo (x, y, r, l, l2, l3, n, p, q, px, s, omp_default_mem_alloc, 0);
  x = 42; y = 0; r = 0; l = -1; l2[0] = -1; l2[1] = -1;
  l2[2] = -1; l2[3] = -1; n = 8;
  foo (x, y, r, l, l2, l3, n, p, q, px, s, a, 1);
  x = 42; y = 0; r = 0; l = -1; l2[0] = -1; l2[1] = -1;
  l2[2] = -1; l2[3] = -1; n = 8;
  omp_set_default_allocator (a);
  foo (x, y, r, l, l2, l3, n, p, q, px, s, omp_null_allocator, 3);
  x = 42; y = 0; r = 0; l = -1; l2[0] = -1; l2[1] = -1;
  l2[2] = -1; l2[3] = -1; n = 8;
  foo (x, y, r, l, l2, l3, n, p, q, px, s, omp_default_mem_alloc, 2);
  omp_destroy_allocator (a);
  return 0;
}
