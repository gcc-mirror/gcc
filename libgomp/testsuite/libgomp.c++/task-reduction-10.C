extern "C" void abort ();

struct S { S (); S (long long int, int); ~S (); static int cnt1, cnt2, cnt3; long long int s; int t; };

int S::cnt1;
int S::cnt2;
int S::cnt3;

S::S ()
{
  #pragma omp atomic
  cnt1++;
}

S::S (long long int x, int y) : s (x), t (y)
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

S as = { 0LL, 7 };
S &a = as;
S bs (1LL, 5);
S &b = bs;

void
foo (S &c, S &d)
{
  int i;
  for (i = 0; i < 2; i++)
    #pragma omp task in_reduction (+: c) in_reduction (*: b, d) in_reduction (+: a)
    {
      a.s += 7;
      b.s *= 2;
      c.s += 9;
      d.s *= 3;
      if ((a.t != 7 && a.t != 3) || (b.t != 5 && b.t != 9)
	  || (c.t != 7 && c.t != 3) || (d.t != 5 && d.t != 9))
	abort ();
    }
}

void
test ()
{
  S cs = { 0LL, 7 };
  S &c = cs;
  S ds (1LL, 5);
  #pragma omp parallel if (0)
  {
    S &d = ds;
    #pragma omp parallel reduction (task, +: a, c) reduction (task, *: b, d)
    {
      #pragma omp for
      for (int i = 0; i < 4; i++)
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
	      foo (c, d);
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
#define THREEP7 (3LL * 3LL * 3LL * 3LL * 3LL * 3LL * 3LL)
    if (d.s != (THREEP7 * THREEP7 * THREEP7 * THREEP7) || d.t != 5)
      abort ();
  }
  if (a.s != 28 * 7 || a.t != 7 || b.s != (1L << 28) || b.t != 5
      || c.s != 28 * 9 || c.t != 7)
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
