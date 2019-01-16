#include <omp.h>
#include <stdlib.h>

struct S { unsigned long long int s, t; };

void
rbar (struct S *p, struct S *o)
{
  p->s = 1;
  if (o->t != 5)
    abort ();
  p->t = 9;
}

static inline void
rbaz (struct S *o, struct S *i)
{
  if (o->t != 5 || i->t != 9)
    abort ();
  o->s *= i->s;
}

#pragma omp declare reduction (+: struct S : omp_out.s += omp_in.s) \
  initializer (omp_priv = { 0, 3 })
#pragma omp declare reduction (*: struct S : rbaz (&omp_out, &omp_in)) \
  initializer (rbar (&omp_priv, &omp_orig))

struct S g = { 0, 7 };
struct S h = { 1, 5 };

int
foo (int z, int *a, int *b)
{
  int x = 0;
  #pragma omp taskloop reduction (+:x) in_reduction (+:b[0])
  for (int i = z; i < z + 8; i++)
    {
      x += a[i];
      *b += a[i] * 2;
    }
  return x;
}

unsigned long long int
bar (int z, int *a, unsigned long long int *b, int *s)
{
  unsigned long long int x = 1;
  #pragma omp taskloop reduction (*:x) in_reduction (*:b[0]) \
		       in_reduction (+:s[0])
  for (int i = z; i < z + 8; i++)
    {
      #pragma omp task in_reduction (*:x)
      x *= a[i];
      #pragma omp task in_reduction (*:b[0])
      *b *= (3 - a[i]);
      s[0]++;
    }
  return x;
}

void
baz (int i, int *a, int *c)
{
  #pragma omp task in_reduction (*:h) in_reduction (+:g)
  {
    g.s += 7 * a[i];
    h.s *= (3 - c[i]);
    if ((g.t != 7 && g.t != 3) || (h.t != 5 && h.t != 9))
      abort ();
  }
}

int
main ()
{
  int i, j = 0, a[64], b = 0, c[64], f = 0;
  unsigned long long int d = 1, e = 1;
  volatile int one = 1;
  int r = 0, s = 0, t;
  struct S m = { 0, 7 };
  struct S n = { 1, 5 };
  for (i = 0; i < 64; i++)
    {
      a[i] = 2 * i;
      c[i] = 1 + ((i % 3) != 1);
    }
  #pragma omp parallel reduction (task, +:b) shared(t) reduction(+:r, s)
  {
    int z, q1, q2, q3;
    #pragma omp master
    t = omp_get_num_threads ();
    #pragma omp for schedule(static) reduction (task, +: f) reduction (+: j)
    for (z = 0; z < 64; z += 8)
      {
	f++;
	j += foo (z, a, &b);
	j += foo (z, a, &f);
      }
    if (j != 63 * 64 * 2 || f != 63 * 64 * 2 + 8)
      abort ();
    r++;
    #pragma omp taskgroup task_reduction (+: s)
    {
      #pragma omp for schedule(static, 1) reduction(task, *: d) reduction (*: e)
      for (z = 0; z < 64; z += 8)
	e *= bar (z, c, &d, &s);
    }
    if (e != (1ULL << 43) || d != (1ULL << 21))
      abort ();
    #pragma omp for schedule(monotonic: dynamic, 1) reduction (task, +: g, m) \
		    reduction (task, *: h, n) collapse(3)
    for (q1 = 0; q1 < one; q1++)
      for (q2 = 0; q2 < 64; q2 += 8)
	for (q3 = 0; q3 < one; ++q3)
	  #pragma omp taskloop in_reduction (+: g, m) in_reduction (*: h, n) \
			       nogroup
	  for (i = q2; i < q2 + 8; ++i)
	    {
	      g.s += 3 * a[i];
	      h.s *= (3 - c[i]);
	      m.s += 4 * a[i];
	      n.s *= c[i];
	      if ((g.t != 7 && g.t != 3) || (h.t != 5 && h.t != 9)
		  || (m.t != 7 && m.t != 3) || (n.t != 5 && n.t != 9))
		abort ();
	      baz (i, a, c);
	    }
    if (n.s != (1ULL << 43) || n.t != 5)
      abort ();
    if (g.s != 63 * 64 * 10 || g.t != 7)
      abort ();
    if (h.s != (1ULL << 42) || h.t != 5)
      abort ();
    if (m.s != 63 * 64 * 4 || m.t != 7)
      abort ();
  }
  if (b != 63 * 64 * 2)
    abort ();
  if (r != t || s != 64)
    abort ();
  return 0;
}
