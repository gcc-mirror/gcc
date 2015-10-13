// { dg-do run }

struct R { R () {}; ~R () {}; int r; };
struct T { T () {}; virtual ~T () {}; int t; };
int c;
struct A : public R, virtual public T { A () : b(c) {} int a; int &b; void m1 (); };
int d[64];

void
A::m1 ()
{
  r = 0;
  #pragma omp parallel for private (a) reduction(|:R::r)
  for (a = 0; A::a < 31; a += 2)
    r |= (1 << A::a);
  if (r != 0x55555555)
    __builtin_abort ();
  #pragma omp parallel for simd linear (R::r)
  for (R::r = 0; r < 32; R::r++)
    d[r + 8] |= 1;
  for (int i = 0; i < 64; i++)
    if (d[i] != ((i >= 8 && i < 32 + 8) ? 1 : 0))
      __builtin_abort ();
  #pragma omp parallel for lastprivate (t)
  for (T::t = 0; t < 32; t += 3)
    d[T::t + 2] |= 2;
  if (T::t != 33)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 8 && i < 32 + 8) ? 1 : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)))
      __builtin_abort ();
  #pragma omp simd linear (t)
  for (t = 0; t < 32; t++)
    d[T::t + 9] |= 4;
  if (t != 32)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 8 && i < 32 + 8) ? 1 : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)))
      __builtin_abort ();
  r = 0;
  #pragma omp parallel for reduction(|:r)
  for (a = 0; A::a < 31; a += 2)
    r |= (1 << A::a);
  if (r != 0x55555555)
    __builtin_abort ();
  #pragma omp parallel for simd
  for (R::r = 0; r < 32; R::r += 2)
    d[r + 8] |= 8;
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)))
      __builtin_abort ();
  #pragma omp simd collapse(2)
  for (T::t = 0; t < 7; t += 2)
    for (a = 0; A::a < 8; a++)
      d[((t << 2) | a) + 3] |= 16;
  if (t != 8 || A::a != 8)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)
		 | ((i >= 3 && i < 32 + 3) ? 16 : 0)))
      __builtin_abort ();
  T::t = 32;
  a = 16;
  #pragma omp parallel
  #pragma omp single
  #pragma omp taskloop simd collapse(2)
  for (t = 0; T::t < 7; T::t += 2)
    for (A::a = 0; a < 8; A::a++)
      d[((t << 2) | A::a) + 3] |= 32;
  if (T::t != 8 || a != 8)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)
		 | ((i >= 3 && i < 32 + 3) ? (16 | 32) : 0)))
      __builtin_abort ();
  #pragma omp parallel
  #pragma omp single
  #pragma omp taskloop simd
  for (R::r = 0; r < 31; R::r += 2)
    d[r + 8] |= 64;
  if (r != 32)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (64 | 8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)
		 | ((i >= 3 && i < 32 + 3) ? (16 | 32) : 0)))
      __builtin_abort ();
}

int
main ()
{
  A a;
  a.m1 ();
}
