/* PR c/64824 */
/* { dg-do run } */

void
f1 (void)
{
  short a;
  short b = 1;
  int c = 3;
#pragma omp atomic capture
  a = b = c << b;
  if (b != 6 || a != 6)
    __builtin_abort ();
}

void
f2 (void)
{
  short a;
  short b = 1;
  int c = 3;
#pragma omp atomic capture
  a = b = c + b;
  if (b != 4 || a != 4)
    __builtin_abort ();
}

void
f3 (void)
{
  short a;
  short b = 1;
  long long int c = 3;
#pragma omp atomic capture
  a = b = c + b;
  if (b != 4 || a != 4)
    __builtin_abort ();
}

void
f4 (void)
{
  char a;
  char b = 1;
  long long int c = 3LL;
#pragma omp atomic capture
  a = b = c << b;
  if (b != 6 || a != 6)
    __builtin_abort ();
}

int
main ()
{
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  return 0;
}
