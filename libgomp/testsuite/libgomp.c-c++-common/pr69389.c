/* PR c/69389 */

struct S { unsigned int a : 10; unsigned int b : 4; unsigned int c : 18; } s = { 210, 11, 1235 };

unsigned int
f1 (void)
{
  unsigned int v;
  #pragma omp atomic read
  v = s.b;
  return v;
}

void
f2 (unsigned int v)
{
  #pragma omp atomic write
  s.b = v;
}

void
f3 (void)
{
  #pragma omp atomic
  s.b |= 1;
}

int
f4 (void)
{
  int v;
  #pragma omp atomic capture
  v = s.b += 8;
  return v;
}

int
f5 (void)
{
  int v;
  #pragma omp atomic capture
  {
    v = s.b;
    s.b -= 4;
  }
  return v;
}

void
f6 (void)
{
  #pragma omp atomic
  s.b = s.b & 7;
}

void
f7 (void)
{
  #pragma omp atomic
  s.b = ~7 & s.b;
}

int
f8 (void)
{
  int v;
  #pragma omp atomic capture
  v = s.b = 8 + s.b;
  return v;
}

int
f9 (void)
{
  int v;
  #pragma omp atomic capture
  {
    v = s.b;
    s.b = s.b - 4;
  }
  return v;
}

int
main ()
{
  if (f1 () != 11)
    __builtin_abort ();
  f2 (4);
  if (s.a != 210 || s.b != 4 || s.c != 1235)
    __builtin_abort ();
  s.a = 813;
  s.c = 31532;
  if (f1 () != 4)
    __builtin_abort ();
  f3 ();
  if (f1 () != 5)
    __builtin_abort ();
  if (s.a != 813 || s.b != 5 || s.c != 31532)
    __builtin_abort ();
  if (f4 () != 13)
    __builtin_abort ();
  if (f1 () != 13)
    __builtin_abort ();
  f2 (14);
  if (s.a != 813 || s.b != 14 || s.c != 31532)
    __builtin_abort ();
  if (f5 () != 14)
    __builtin_abort ();
  if (f1 () != 10 || s.a != 813 || s.b != 10 || s.c != 31532)
    __builtin_abort ();
  f6 ();
  if (f1 () != 2)
    __builtin_abort ();
  f2 (15);
  f7 ();
  if (f1 () != 8)
    __builtin_abort ();
  if (f8 () != 0 || s.a != 813 || s.b != 0 || s.c != 31532)
    __builtin_abort ();
  if (f9 () != 0 || s.a != 813 || s.b != 12 || s.c != 31532)
    __builtin_abort ();
  return 0;
}
