extern "C" void abort ();
int w;
struct S {
  int s, &t;
  int *p;
  S (int *x) : s (0), t (w), p(x) {};
  void foo (short &);
  void bar (short &);
  void baz (short &);
  void qux (short &);
};

void
S::foo (short &x)
{
  #pragma omp simd lastprivate (conditional: x, s, t)
  for (int i = 0; i < 1025; ++i)
    {
      if (p[i])
	x = i;
      if (p[i + 1025])
	s = i + 3;
      if (p[i + 2 * 1025])
	t = i + 6;
    }
}

void
S::bar (short &x)
{
  #pragma omp simd lastprivate (conditional: x, s, t) collapse (3) if (0)
  for (int i = 0; i < 15; ++i)
    for (int j = -4; j < 9; j++)
      for (int k = 12; k > 7; --k)
	{
	  int l = (k - 8) + (j + 4) * 5 + i * 13 * 5;
	  if (p[l])
	    x = l;
	  if (p[l + 1025])
	    s = l + 3;
	  if (p[l + 1025 * 2])
	    t = l + 6;
	}
}

void
S::baz (short &x)
{
  #pragma omp parallel for simd lastprivate (conditional: x, s, t) if (simd: 0)
  for (int i = 0; i < 1025; ++i)
    {
      if (p[i])
	x = i;
      if (p[i + 1025])
	s = i + 3;
      if (p[i + 2 * 1025])
	t = i + 6;
    }
}

void
S::qux (short &x)
{
  #pragma omp for simd lastprivate (conditional: x, s, t) collapse (3) schedule (simd: guided, 8)
  for (int i = 0; i < 15; ++i)
    for (int j = -4; j < 9; j++)
      for (int k = 12; k > 7; --k)
	{
	  int l = (k - 8) + (j + 4) * 5 + i * 13 * 5;
	  if (p[l])
	    x = l;
	  if (p[l + 1025])
	    s = l + 3;
	  if (p[l + 1025 * 2])
	    t = l + 6;
	}
}

int
main ()
{
  short x;
  int a[3 * 1025];
  for (int i = 0; i < 1025; ++i)
    {
      a[i] = ((i % 17) == 7);
      a[1025 + i] = ((i % 19) == 2);
      a[2 * 1025 + i] = ((i % 23) == 5);
    }
  S s = a;
  s.foo (x);
  if (x != 1010 || s.s != 1012 || s.t != 1023)
    abort ();
  s.bar (x);
  if (x != 959 || s.s != 974 || s.t != 977)
    abort ();
  #pragma omp parallel
  s.baz (x);
  if (x != 1010 || s.s != 1012 || s.t != 1023)
    abort ();
  s.qux (x);
  if (x != 959 || s.s != 974 || s.t != 977)
    abort ();
}
