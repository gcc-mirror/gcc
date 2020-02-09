#define A(n) n##0, n##1, n##2, n##3, n##4, n##5, n##6, n##7, n##8, n##9
#define B(n) A(n##0), A(n##1), A(n##2), A(n##3), A(n##4), A(n##5), A(n##6), A(n##7), A(n##8), A(n##9)

int
foo (int x)
{
  int b[] = { B(4), B(5), B(6) };
  return b[x];
}

int v[] = { 1, 2, 3, 4, 5, 6 };
#pragma omp declare target to (foo, v)

int
main ()
{
  int i = 5;
  asm ("" : "+g" (i));
  #pragma omp target map(tofrom:i)
  {
    int a[] = { B(1), B(2), B(3) };
    asm ("" : : "m" (a) : "memory");
    i = a[i] + foo (i) + v[i & 63];
  }
  if (i != 105 + 405 + 6)
    __builtin_abort ();
  return 0;
}
