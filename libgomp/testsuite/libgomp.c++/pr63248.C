// PR c++/63248
// { dg-do run }

int *v;

template <typename T>
T
foo (T A, T B)
{
  T a = 2;
  T b = 4;

#pragma omp target map(v[a:b])
  v[a] = 1;

#pragma omp target map(v[A:B])
  v[a] = 2;

#pragma omp target map(A)
  A = 19;
  return A;
}

template <int N>
int
bar (int A, int B)
{
#pragma omp target map(A)
  A = 8;
  if (A != 8)
    __builtin_abort ();
#pragma omp target map(A, B)
  {
    A = 1;
    B = 2;
  }
  return A + B;
}

int
baz (int A, int B)
{
#pragma omp target map(A)
  A = 8;
  if (A != 8)
    __builtin_abort ();
#pragma omp target map(A, B)
  {
    A = 1;
    B = 2;
  }
  return A + B;
}

int
main ()
{
  int a[10] = { 0 };
  v = a;
  if (foo (1, 5) != 19 || v[2] != 2 || bar<0> (5, 7) != 3 || baz (5, 7) != 3)
    __builtin_abort ();
}
