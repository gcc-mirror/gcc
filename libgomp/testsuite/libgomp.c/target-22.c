extern void abort (void);
struct T { int a; int *b; int c; };
struct S { int *s; char *u; struct T v; short *w; };
volatile int z;

int
main ()
{
  struct S s;
  int a[32], i;
  char b[32];
  short c[32];
  for (i = 0; i < 32; i++)
    {
      a[i] = i;
      b[i] = 32 + i;
      c[i] = 64 + i;
    }
  s.s = a;
  s.u = b + 2;
  s.v.b = a + 16;
  s.w = c + 3;
  int err = 0;
  #pragma omp target map (to:s.v.b[0:z + 7], s.u[z + 1:z + 4]) \
		     map (tofrom:s.s[3:3]) \
		     map (from: s.w[z:4], err) private (i)
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
    for (i = 0; i < 4; i++)
      s.w[i] = 96 + i;
  }
  if (err)
    abort ();
  for (i = 0; i < 32; i++)
    if (a[i] != ((i >= 3 && i < 6) ? 128 + i : i)
	|| b[i] != 32 + i
	|| c[i] != ((i >= 3 && i < 7) ? 93 + i : 64 + i))
      abort ();
  return 0;
}
