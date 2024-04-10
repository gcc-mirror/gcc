/* PR c++/110347 */

#include <omp.h>

struct t {
  int A;
  void f (int dev);
};

void
t::f (int dev)
{
  int B = 49;

  A = 7;
  #pragma omp parallel private(A) if(0) shared(B) default(none)
  {
    A = 5;
    B = A;
  }
  if (A != 7) { __builtin_printf("ERROR 1: %d (!= 7) omp parallel\n", A); __builtin_abort (); }
  if (B != 5) { __builtin_printf("ERROR 1a: %d\n", B); __builtin_abort (); }
  A = 8; B = 49;
  #pragma omp parallel private(A)if(0) shared(B) default(none)
  {
    A = 6;
    B = A;
  }
  if (A != 8) { __builtin_printf("ERROR 2: %d (!= 8) omp parallel\n", A); __builtin_abort (); }
  if (B != 6) { __builtin_printf("ERROR 2a: %d\n", B); __builtin_abort (); }
  A = 8; B = 49;

  #pragma omp target private(A) map(from:B) device(dev)
  {
    A = 7;
    B = A;
  }
  if (A != 8) { __builtin_printf("ERROR 3: %d (!= 8) omp target\n", A); __builtin_abort (); }
  if (B != 7) { __builtin_printf("ERROR 3a: %d\n", B); __builtin_abort (); }
  A = 9; B = 49;
  #pragma omp target private(A) map(from:B) device(dev)
  {
    A = 8;
    B = A;
  }
  if (A != 9) { __builtin_printf("ERROR 4: %d (!= 9) omp target\n", A); __builtin_abort (); }
  if (B != 8) { __builtin_printf("ERROR 4a: %d\n", B); __builtin_abort (); }
}


template <typename T>
struct tt {
  T C;
  void g (int dev);
};

template <typename T>
void
tt<T>::g (int dev)
{
  T D = 49;
  C = 7;
  #pragma omp parallel private(C) if(0) shared(D) default(none)
  {
    C = 5;
    D = C;
  }
  if (C != 7) { __builtin_printf("ERROR 1: %d (!= 7) omp parallel\n", C);__builtin_abort (); }
  if (D != 5) { __builtin_printf("ERROR 1a: %d\n", D);__builtin_abort (); }
  C = 8; D = 49;
  #pragma omp parallel private(C)if(0) shared(D) default(none)
  {
    C = 6;
    D = C;
  }
  if (C != 8) { __builtin_printf("ERROR 2: %d (!= 8) omp parallel\n", C);__builtin_abort (); }
  if (D != 6) { __builtin_printf("ERROR 2a: %d\n", D);__builtin_abort (); }
  C = 8; D = 49;
  #pragma omp target private(C) map(from:D) defaultmap(none) device(dev)
  {
    C = 7;
    D = C;
  }
  if (C != 8) { __builtin_printf("ERROR 3: %d (!= 8) omp target\n", C);__builtin_abort (); }
  if (D != 7) { __builtin_printf("ERROR 3a: %d\n", D);__builtin_abort (); }
  C = 9; D = 49;
  #pragma omp target private(C) map(from:D) defaultmap(none) device(dev)
  {
    C = 8;
    D = C;
  }
  if (C != 9) { __builtin_printf("ERROR 4: %d (!= 9) omp target\n", C); __builtin_abort (); }
  if (D != 8) { __builtin_printf("ERROR 4a: %d\n", D); }
}

void
foo ()
{
  struct t x;
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    x.f (dev);
}

void
bar ()
{
  struct tt<int> y;
  for (int dev = 0; dev <= omp_get_num_devices (); dev++)
    y.g (dev);
}

int
main ()
{
  foo ();
  bar ();
}
