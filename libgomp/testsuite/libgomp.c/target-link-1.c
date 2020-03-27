struct S { int s, t; };

int a = 1, b = 1;
double c[27];
struct S d = { 8888, 8888 };
#pragma omp declare target link (a) to (b) link (c, d)

int
foo (void)
{
  return a++ + b++;
}

int
bar (int n)
{
  int *p1 = &a;
  int *p2 = &b;
  c[n] += 2.0;
  d.s -= 2;
  d.t -= 2;
  return *p1 + *p2 + d.s + d.t;
}

#pragma omp declare target (foo, bar)

int
main ()
{
  a = b = 2;
  d.s = 17;
  d.t = 18;

  int res, n = 10;
  #pragma omp target map (to: a, b, c, d) map (from: res)
  {
    res = foo () + foo ();
    c[n] = 3.0;
    res += bar (n);
  }

  int shared_mem = 0;
  #pragma omp target map (alloc: shared_mem)
    shared_mem = 1;

  if ((shared_mem && res != (2 + 2) + (3 + 3) + (4 + 4 + 15 + 16))
      || (!shared_mem && res != (2 + 1) + (3 + 2) + (4 + 3 + 15 + 16)))
    __builtin_abort ();

  #pragma omp target enter data map (to: c)
  #pragma omp target update from (c)
  res = (int) (c[n] + 0.5);
  if ((shared_mem && res != 5) || (!shared_mem && res != 0))
    __builtin_abort ();

  #pragma omp target map (to: a, b) map (from: res)
    res = foo ();

  if ((shared_mem && res != 4 + 4) || (!shared_mem && res != 2 + 3))
    __builtin_abort ();

  return 0;
}
