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
foo (int *a, int *b)
{
  int x = 0;
  #pragma omp taskloop reduction (+:x) in_reduction (+:b[0])
  for (int i = 0; i < 64; i++)
    {
      x += a[i];
      *b += a[i] * 2;
    }
  return x;
}

unsigned long long int
bar (int *a, unsigned long long int *b)
{
  unsigned long long int x = 1;
  #pragma omp taskloop reduction (*:x) in_reduction (*:b[0])
  for (int i = 0; i < 64; i++)
    {
      #pragma omp task in_reduction (*:x)
      x *= a[i];
      #pragma omp task in_reduction (*:b[0])
      *b *= (3 - a[i]);
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
  int i, j, a[64], b = 0, c[64];
  unsigned long long int d = 1, e;
  int r = 0, t;
  struct S m = { 0, 7 };
  struct S n = { 1, 5 };
  for (i = 0; i < 64; i++)
    {
      a[i] = 2 * i;
      c[i] = 1 + ((i % 3) != 1);
    }
  #pragma omp parallel reduction (task, +:b) reduction(+:r) \
		       reduction(task,*:d) reduction (task, +: g, m) \
		       reduction (task, *: h, n) shared(t)
  {
    #pragma omp master
    {
      j = foo (a, &b);
      t = omp_get_num_threads ();
    }
    r++;
    #pragma omp single nowait
      e = bar (c, &d);
    #pragma omp master
    #pragma omp taskloop in_reduction (+: g, m) in_reduction (*: h, n)
    for (i = 0; i < 64; ++i)
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
  }
  if (n.s != (1ULL << 43) || n.t != 5)
    abort ();
  if (j != 63 * 64 || b != 63 * 64 * 2)
    abort ();
  if (e != (1ULL << 43) || d != (1ULL << 21))
    abort ();
  if (g.s != 63 * 64 * 10 || g.t != 7)
    abort ();
  if (h.s != (1ULL << 42) || h.t != 5)
    abort ();
  if (m.s != 63 * 64 * 4 || m.t != 7)
    abort ();
  if (r != t)
    abort ();
  return 0;
}
