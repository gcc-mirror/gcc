// { dg-do run }

template <typename T>
struct A
{
  A () { t = 0; }
  A (T x) { t = x; }
  A (const A &x) { t = x.t; }
  ~A () {}
  T t;
};
template <typename T>
struct M
{
  M () { t = 1; }
  M (T x) { t = x; }
  M (const M &x) { t = x.t; }
  ~M () {}
  T t;
};
template <typename T>
struct B
{
  B () { t = ~(T) 0; }
  B (T x) { t = x; }
  B (const B &x) { t = x.t; }
  ~B () {}
  T t;
};
template <typename T>
void
add (T &x, T &y)
{
  x.t += y.t;
}
template <typename T>
void
zero (T &x)
{
  x.t = 0;
}
template <typename T>
void
orit (T *x, T *y)
{
  y->t |= x->t;
}
B<long> bb;
#pragma omp declare reduction(+:A<int>:omp_out.t += omp_in.t)
#pragma omp declare reduction(+:A<char>:add (omp_out, omp_in)) initializer(zero (omp_priv))
#pragma omp declare reduction(*:M<int>:omp_out.t *= omp_in.t) initializer(omp_priv = 1)
#pragma omp declare reduction(|:A<unsigned long long>:orit (&omp_in, &omp_out))
#pragma omp declare reduction(&:B<long>:omp_out.t = omp_out.t & omp_in.t) initializer(orit (&omp_priv, &omp_orig))
#pragma omp declare reduction(maxb:short:omp_out = omp_in > omp_out ? omp_in : omp_out) initializer(omp_priv = -6)

A<char> z[10];

template <int N>
__attribute__((noinline, noclone)) void
foo (A<int> (*&x)[3][N], M<int> *y, B<long> (&w)[1][N], int p1, long p2, long p3, int p4,
     int p5, long p6, short p7, int s, int t)
{
  A<unsigned long long> a[p7 + 4];
  short bb[p7];
  short (&b)[p7] = bb;
  for (int i = 0; i < p7; i++)
    bb[i] = -6;
  #pragma omp parallel for reduction(+:x[-1:p1 + 1][:p2 + N - 2], z[t + N:p3]) \
			   reduction(*:y[-s:p4]) reduction(|:a[s + 3:p5 - N + 2]) \
			   reduction(&:w[s + 1:p6 - 3 + N][t:p6]) reduction(maxb:b[N:])
  for (int i = 0; i < 128; i++)
    {
      x[i / 64 - 1][i % 3][(i / 4) & 1].t += i;
      if ((i & 15) == 1)
	y[1].t *= 3;
      if ((i & 31) == N)
	y[2].t *= 7;
      if ((i & 63) == 3)
	y[N + 1].t *= 17;
      z[i / 32 + 2].t += (i & 3);
      if (i < 4)
	z[i + N].t += i;
      a[i / 32 + 2].t |= 1ULL << (i & 30);
      w[0][i & 1].t &= ~(1L << (i / 17 * 3));
      if ((i % 23) > b[N])
	b[N] = i % 23;
      if ((i % 85) > b[3])
	b[3] = i % 85;
      if ((i % 192) > b[4])
	b[4] = i % 192;
    }
  for (int i = 0; i < 9; i++)
    if (a[i].t != ((i < 6 && i >= 2) ? 0x55555555ULL : 0))
      __builtin_abort ();
  if (bb[0] != -6 || bb[1] != -6 || bb[N] != 22 || bb[3] != 84 || bb[4] != 127)
    __builtin_abort ();
}

A<int> a3[4][3][2];
A<int> (*p3)[3][2] = &a3[2];
M<int> y3[5] = { 0, 1, 1, 1, 0 };
B<long> w3[1][2];

template <int N>
struct S
{
  A<int> (*&x)[3][N];
  M<int> *y;
  B<long> (&w)[1][N];
  A<char> z[10];
  short b[5];
  A<unsigned long long> a[9];
  S() : x(p3), y(y3), w(w3), z(), a(), b() {}
  __attribute__((noinline, noclone)) void foo (int, long, long, int, int, long, short, int, int);
};

template <int N>
void
S<N>::foo (int p1, long p2, long p3, int p4, int p5, long p6, short p7, int s, int t)
{
  #pragma omp parallel for reduction(+:x[-1:p1 + 1][:p2][0:N], z[t + N:p3 + N - 2]) \
			   reduction(*:y[-s:p4]) reduction(|:a[s + 3:p5]) \
			   reduction(&:w[s + 1:p6 - 3 + N][t:p6]) reduction(maxb:b[N:])
  for (int i = 0; i < 128; i++)
    {
      x[i / 64 - 1][i % 3][(i / 4) & 1].t += i;
      if ((i & 15) == 1)
	y[1].t *= 3;
      if ((i & 31) == N)
	y[2].t *= 7;
      if ((i & 63) == 3)
	y[N + 1].t *= 17;
      z[i / 32 + 2].t += (i & 3);
      if (i < 4)
	z[i + N].t += i;
      a[i / 32 + 2].t |= 1ULL << (i & 30);
      w[0][i & 1].t &= ~(1L << (i / 17 * 3));
      if ((i % 23) > b[N])
	b[N] = i % 23;
      if ((i % 85) > b[3])
	b[3] = i % 85;
      if ((i % 192) > b[4])
	b[4] = i % 192;
    }
}

int
main ()
{
  A<int> a[4][3][2];
  static int a2[4][3][2] = {{{ 0, 0 }, { 0, 0 }, { 0, 0 }},
			    {{ 312, 381 }, { 295, 356 }, { 337, 335 }},
			    {{ 1041, 975 }, { 1016, 1085 }, { 935, 1060 }},
			    {{ 0, 0 }, { 0, 0 }, { 0, 0 }}};
  A<int> (*p)[3][2] = &a[2];
  M<int> y[5] = { 0, 1, 1, 1, 0 };
  int y2[5] = { 0, 6561, 2401, 289, 0 };
  char z2[10] = { 0, 0, 48, 49, 50, 51, 0, 0, 0, 0 };
  B<long> w[1][2];
  foo<2> (p, y, w, 1, 3L, 4L, 3, 4, 2L, 5, -1, 0);
  for (int i = 0; i < 4; i++)
    for (int j = 0; j < 3; j++)
      for (int k = 0; k < 2; k++)
	if (a[i][j][k].t != a2[i][j][k])
	  __builtin_abort ();
  for (int i = 0; i < 5; i++)
    if (y[i].t != y2[i])
      __builtin_abort ();
  for (int i = 0; i < 10; i++)
    if (z[i].t != z2[i])
      __builtin_abort ();
  if (w[0][0].t != ~0x249249L || w[0][1].t != ~0x249249L)
    __builtin_abort ();
  S<2> s;
  s.foo (1, 3L, 4L, 3, 4, 2L, 5, -1, 0);
  for (int i = 0; i < 9; i++)
    if (s.a[i].t != ((i < 6 && i >= 2) ? 0x55555555ULL : 0))
      __builtin_abort ();
  for (int i = 0; i < 4; i++)
    for (int j = 0; j < 3; j++)
      for (int k = 0; k < 2; k++)
	if (a3[i][j][k].t != a2[i][j][k])
	  __builtin_abort ();
  for (int i = 0; i < 5; i++)
    if (y3[i].t != y2[i])
      __builtin_abort ();
  for (int i = 0; i < 10; i++)
    if (s.z[i].t != z2[i])
      __builtin_abort ();
  if (w3[0][0].t != ~0x249249L || w3[0][1].t != ~0x249249L)
    __builtin_abort ();
  if (s.b[0] != 0 || s.b[1] != 0 || s.b[2] != 22
      || s.b[3] != 84 || s.b[4] != 127)
    __builtin_abort ();
}
