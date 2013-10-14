// { dg-do run }

extern "C" void abort ();

struct S
{
  int s;
  void foo (S &x) { s += x.s; }
  S (const S &x) { s = x.s + 1; }
  S () { s = 6; }
  ~S () {}
};

void
bar (S &x, S &y)
{
  if (x.s != 6 || y.s != 6)
    abort ();
  x.s = 8;
}

#pragma omp declare reduction (foo: S: omp_out.foo (omp_in)) \
	initializer (omp_priv (omp_orig))
#pragma omp declare reduction (bar : S: omp_out.foo (omp_in)) \
	initializer (bar (omp_priv, omp_orig))

S
baz (S x)
{
  S r;
  int i = 0;
  if (x.s != 7 || r.s != 6)
    abort ();
  #pragma omp parallel reduction (foo: x) reduction (bar: r) \
		       reduction (+: i)
  {
    if (x.s != 8 || r.s != 8)
      abort ();
    x.s = 12;
    r.s = 14;
    i = 1;
  }
  if (x.s != 7 + 12 * i || r.s != 6 + 14 * i)
    abort ();
  return r;
}

void
baz (S &x, S &y)
{
  int i = 0, &j = i;
  #pragma omp parallel reduction (foo: x) reduction (bar: y) \
		       reduction (+: i)
  {
    if (x.s != 7 || y.s != 8)
      abort ();
    x.s = 12;
    y.s = 14;
    i = 1;
  }
  if (x.s != 6 + 12 * j || y.s != 6 + 14 * j)
    abort ();
}

int
main ()
{
  S s;
  baz (s);
  S t, u;
  baz (t, u);
}
