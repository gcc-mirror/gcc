// { dg-do run }

int c, d, e;
struct R { R () {}; ~R () {}; int r; };
template <typename Q>
struct T { T () : t(d) {}; virtual ~T () {}; Q t; };
template <typename Q>
struct A : public R, virtual public T<Q> { A () : b(c), a(e) {} Q a; int &b; void m1 (); };
int f[64];

template <typename Q>
void
A<Q>::m1 ()
{
  r = 0;
  #pragma omp parallel for private (a) reduction(|:R::r)
  for (a = 0; A::a < 31; a += 2)
    r |= (1 << A::a);
  if (r != 0x55555555)
    __builtin_abort ();
  #pragma omp parallel for simd linear (R::r)
  for (R::r = 0; r < 32; R::r++)
    f[r + 8] |= 1;
  for (int i = 0; i < 64; i++)
    if (f[i] != ((i >= 8 && i < 32 + 8) ? 1 : 0))
      __builtin_abort ();
  #pragma omp parallel for lastprivate (T<Q>::t)
  for (T<Q>::t = 0; T<Q>::t < 32; T<Q>::t += 3)
    f[T<Q>::t + 2] |= 2;
  if (T<Q>::t != 33)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (f[i] != (((i >= 8 && i < 32 + 8) ? 1 : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)))
      __builtin_abort ();
  #pragma omp simd linear (T<Q>::t)
  for (T<Q>::t = 0; T<Q>::t < 32; T<Q>::t++)
    f[T<Q>::t + 9] |= 4;
  if (T<Q>::t != 32)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (f[i] != (((i >= 8 && i < 32 + 8) ? 1 : 0)
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
    f[r + 8] |= 8;
  for (int i = 0; i < 64; i++)
    if (f[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)))
      __builtin_abort ();
  #pragma omp simd collapse(2)
  for (T<Q>::t = 0; T<Q>::t < 7; T<Q>::t += 2)
    for (a = 0; A::a < 8; a++)
      f[((T<Q>::t << 2) | a) + 3] |= 16;
  if (T<Q>::t != 8 || A::a != 8)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (f[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)
		 | ((i >= 3 && i < 32 + 3) ? 16 : 0)))
      __builtin_abort ();
  T<Q>::t = 32;
  a = 16;
  #pragma omp parallel
  #pragma omp single
  #pragma omp taskloop simd collapse(2)
  for (T<Q>::t = 0; T<Q>::t < 7; T<Q>::t += 2)
    for (A::a = 0; a < 8; A::a++)
      f[((T<Q>::t << 2) | A::a) + 3] |= 32;
  if (T<Q>::t != 8 || a != 8)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (f[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)
		 | ((i >= 3 && i < 32 + 3) ? (16 | 32) : 0)))
      __builtin_abort ();
  #pragma omp parallel
  #pragma omp single
  #pragma omp taskloop simd
  for (R::r = 0; r < 31; R::r += 2)
    f[r + 8] |= 64;
  if (r != 32)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (f[i] != (((i >= 8 && i < 32 + 8) ? ((i & 1) ? 1 : (64 | 8 | 1)) : 0)
		 | ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 2 : 0)
		 | ((i >= 9 && i < 32 + 9) ? 4 : 0)
		 | ((i >= 3 && i < 32 + 3) ? (16 | 32) : 0)))
      __builtin_abort ();
}

int
main ()
{
  A<int> a;
  a.m1 ();
}
