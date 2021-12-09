extern "C" void abort ();
struct T { int a; int *b; int c; char (&d)[10]; };
struct S { int *s; char *u; T v; short *w; short *&x; };
volatile int z;

template <typename A, typename B, typename C, typename D>
void
foo ()
{
  A d[10];
  B *e;
  C a[32], i;
  A b[32];
  B c[32];
  for (i = 0; i < 32; i++)
    {
      a[i] = i;
      b[i] = 32 + i;
      c[i] = 64 + i;
    }
  for (i = 0; i < 10; i++)
    d[i] = 17 + i;
  e = c + 18;
  D s = { a, b + 2, { 0, a + 16, 0, d }, c + 3, e };
  int err = 0;
  #pragma omp target map (to: s.v.b, s.v.b[0:z + 7])			\
		     map (s.template u, s.template u[z + 1:z + 4])	\
		     map (tofrom: s.s, s.s[3:3])			\
		     map (tofrom: s. template v. template d[z + 1:z + 3])\
		     map (from: s.w, s.w[z:4], s.x, s.x[1:3], err) private (i)
  {
    err = 0;
    for (i = 0; i < 7; i++)
      if (s.v.b[i] != 16 + i)
	err = 1;
    for (i = 1; i < 5; i++)
      if (s.u[i] != 34 + i)
	err = 1;
    for (i = 3; i < 6; i++)
      if (s.s[i] != i)
	err = 1;
      else
	s.s[i] = 128 + i;
    for (i = 1; i < 4; i++)
      if (s.v.d[i] != 17 + i)
	err = 1;
      else
	s.v.d[i] = 23 + i;
    for (i = 0; i < 4; i++)
      s.w[i] = 96 + i;
    for (i = 1; i < 4; i++)
      s.x[i] = 173 + i;
  }
  if (err)
    abort ();
  for (i = 0; i < 32; i++)
    if (a[i] != ((i >= 3 && i < 6) ? 128 + i : i)
	|| b[i] != 32 + i
	|| c[i] != ((i >= 3 && i < 7) ? 93 + i : ((i >= 19 && i < 22) ? 155 + i : 64 + i)))
      abort ();
  for (i = 0; i < 10; i++)
    if (d[i] != ((i >= 1 && i < 4) ? 23 + i : 17 + i))
      abort ();
}

int
main ()
{
  char d[10];
  short *e;
  int a[32], i;
  char b[32];
  short c[32];
  for (i = 0; i < 32; i++)
    {
      a[i] = i;
      b[i] = 32 + i;
      c[i] = 64 + i;
    }
  for (i = 0; i < 10; i++)
    d[i] = 17 + i;
  e = c + 18;
  S s = { a, b + 2, { 0, a + 16, 0, d }, c + 3, e };
  int err = 0;
  #pragma omp target map (to: s.v.b, s.v.b[0:z + 7], s.u, s.u[z + 1:z + 4]) \
		     map (tofrom: s.s, s.s[3:3], s.v.d[z + 1:z + 3])		\
		     map (from: s.w, s.w[z:4], s.x, s.x[1:3], err) private (i)
  {
    err = 0;
    for (i = 0; i < 7; i++)
      if (s.v.b[i] != 16 + i)
	err = 1;
    for (i = 1; i < 5; i++)
      if (s.u[i] != 34 + i)
	err = 1;
    for (i = 3; i < 6; i++)
      if (s.s[i] != i)
	err = 1;
      else
	s.s[i] = 128 + i;
    for (i = 1; i < 4; i++)
      if (s.v.d[i] != 17 + i)
	err = 1;
      else
	s.v.d[i] = 23 + i;
    for (i = 0; i < 4; i++)
      s.w[i] = 96 + i;
    for (i = 1; i < 4; i++)
      s.x[i] = 173 + i;
  }
  if (err)
    abort ();
  for (i = 0; i < 32; i++)
    if (a[i] != ((i >= 3 && i < 6) ? 128 + i : i)
	|| b[i] != 32 + i
	|| c[i] != ((i >= 3 && i < 7) ? 93 + i : ((i >= 19 && i < 22) ? 155 + i : 64 + i)))
      abort ();
  for (i = 0; i < 10; i++)
    if (d[i] != ((i >= 1 && i < 4) ? 23 + i : 17 + i))
      abort ();
  foo <char, short, int, S> ();
  return 0;
}
