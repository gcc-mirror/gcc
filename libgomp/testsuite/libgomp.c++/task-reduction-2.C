extern "C" void abort ();

struct S { S (); S (long int, long int); ~S (); static int cnt1, cnt2, cnt3; long int s, t; };

int S::cnt1;
int S::cnt2;
int S::cnt3;

S::S ()
{
  #pragma omp atomic
  cnt1++;
}

S::S (long int x, long int y) : s (x), t (y)
{
  #pragma omp atomic update
  ++cnt2;
}

S::~S ()
{
  #pragma omp atomic
  cnt3 = cnt3 + 1;
  if (t < 3 || t > 9 || (t & 1) == 0)
    abort ();
}

void
bar (S *p, S *o)
{
  p->s = 1;
  if (o->t != 5)
    abort ();
  p->t = 9;
}

static inline void
baz (S *o, S *i)
{
  if (o->t != 5 || i->t != 9)
    abort ();
  o->s *= i->s;
}

#pragma omp declare reduction (+: S : omp_out.s += omp_in.s) initializer (omp_priv (0, 3))
#pragma omp declare reduction (*: S : baz (&omp_out, &omp_in)) initializer (bar (&omp_priv, &omp_orig))

S a = { 0, 7 };
S b (1, 5);

void
foo ()
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

void
test ()
{
  S c = { 0, 7 };
  #pragma omp parallel
  #pragma omp single
  {
    S d (1, 5);
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
}

int
main ()
{
  int c1 = S::cnt1, c2 = S::cnt2, c3 = S::cnt3;
  test ();
  if (S::cnt1 + S::cnt2 - c1 - c2 != S::cnt3 - c3)
    abort ();
}
