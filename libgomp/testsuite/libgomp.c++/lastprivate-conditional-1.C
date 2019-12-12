extern "C" void abort ();
int w;
struct S { int s, &t; S () : s (0), t (w) {}; void foo (short &); bool bar (int, int); void baz (short &); };

bool
S::bar (int i, int q)
{
  switch (q)
    {
    case 0: return (i % 17) == 7;
    case 1: return (i % 19) == 2;
    case 2: return (i % 23) == 5;
    default: abort ();
    }
}

void
S::foo (short &x)
{
  #pragma omp for lastprivate (conditional: x, s, t)
  for (int i = 0; i < 1025; ++i)
    {
      if (bar (i, 0))
	x = i;
      if (bar (i, 1))
	s = i + 3;
      if (bar (i, 2))
	t = i + 6;
    }
}

void
S::baz (short &x)
{
  #pragma omp parallel for lastprivate (conditional: x, s, t) collapse (3)
  for (int i = 0; i < 15; ++i)
    for (int j = -4; j < 9; j++)
      for (int k = 12; k > 7; --k)
	{
	  int l = (k - 8) + (j + 4) * 5 + i * 13 * 5;
	  if (bar (l, 0))
	    x = l;
	  if (bar (l, 1))
	    s = l + 3;
	  if (bar (l, 2))
	    t = l + 6;
	}
}

int
main ()
{
  short x;
  S s;
  #pragma omp parallel
  s.foo (x);
  if (x != 1010 || s.s != 1012 || s.t != 1023)
    abort ();
  s.baz (x);
  if (x != 959 || s.s != 974 || s.t != 977)
    abort ();
}
