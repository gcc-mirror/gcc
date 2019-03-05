#ifdef __cplusplus
extern "C"
#endif
void abort (void);

struct S { long int s, t; };

void
bar (struct S *p, struct S *o)
{
  p->s = 1;
  if (o->t != 5)
    abort ();
  p->t = 9;
}

static inline void
baz (struct S *o, struct S *i)
{
  if (o->t != 5 || i->t != 9)
    abort ();
  o->s *= i->s;
}

#pragma omp declare reduction (+: struct S : omp_out.s += omp_in.s) initializer (omp_priv = { 0, 3 })
#pragma omp declare reduction (*: struct S : baz (&omp_out, &omp_in)) initializer (bar (&omp_priv, &omp_orig))

struct S a = { 0, 7 };
struct S b = { 1, 5 };

void
foo (void)
{
  int i;
  for (i = 0; i < 2; i++)
    #pragma omp task in_reduction (*: b) in_reduction (+: a)
    {
      a.s += 7;
      b.s *= 2;
      if ((a.t != 7 && a.t != 3) || (b.t != 5 && b.t != 9))
	abort ();
    }
}

int
main ()
{
  struct S c = { 0, 7 };
  #pragma omp parallel
  #pragma omp single
  {
    struct S d = { 1, 5 };
    #pragma omp taskgroup task_reduction (+: a, c) task_reduction (*: b, d)
    {
      int i;
      for (i = 0; i < 4; i++)
	#pragma omp task in_reduction (*: b, d) in_reduction (+: a, c)
	{
	  int j;
	  a.s += 7;
	  b.s *= 2;
	  for (j = 0; j < 2; j++)
	    #pragma omp task in_reduction (+: a) in_reduction (*: b) \
			     in_reduction (+: c) in_reduction (*: d)
	    {
	      a.s += 7;
	      b.s *= 2;
	      c.s += 9;
	      d.s *= 3;
	      foo ();
	      if ((a.t != 7 && a.t != 3) || (b.t != 5 && b.t != 9)
		  || (c.t != 7 && c.t != 3) || (d.t != 5 && d.t != 9))
		abort ();
	    }
	  c.s += 9;
	  d.s *= 3;
	  if ((a.t != 7 && a.t != 3) || (b.t != 5 && b.t != 9)
	      || (c.t != 7 && c.t != 3) || (d.t != 5 && d.t != 9))
	    abort ();
	}
    }
#define THREEP4 (3L * 3L * 3L * 3L)
    if (d.s != (THREEP4 * THREEP4 * THREEP4) || d.t != 5)
      abort ();
  }
  if (a.s != 28 * 7 || a.t != 7 || b.s != (1L << 28) || b.t != 5
      || c.s != 12 * 9 || c.t != 7)
    abort ();
  return 0;
}
