unsigned short a[256];

__attribute__((noinline, noclone)) void
bar (void *x, unsigned short z)
{
  unsigned short *y = (unsigned short *) x;
  if (y < &a[5] || y > &a[222] || y == &a[124])
    __builtin_abort ();
  *y += z;
}

__attribute__((noinline, noclone)) void
foo (void *qx, void *rx, void *sx, int n)
{
  unsigned short (*q)[n], (*r)[n], (*s)[n], (*p)[n];
  q = (typeof (q)) qx;
  r = (typeof (r)) rx;
  s = (typeof (s)) sx;
  #pragma omp for
  for (p = q; p != r; p++)
    bar (p, 1);
  #pragma omp for
  for (p = s; p != r; p--)
    bar (p, 2);
  #pragma omp for
  for (p = q; p != r; p = p + 1)
    bar (p, 4);
  #pragma omp for
  for (p = s; p != r; p = p - 1)
    bar (p, 8);
  #pragma omp for
  for (p = q; p != r; p = 1 + p)
    bar (p, 16);
  #pragma omp for
  for (p = s; p != r; p = -1 + p)
    bar (p, 32);
  #pragma omp for
  for (p = q; p != r; p += 1)
    bar (p, 64);
  #pragma omp for
  for (p = s; p != r; p -= 1)
    bar (p, 128);
}

__attribute__((noinline, noclone)) void
baz (void *qx, void *rx, void *sx, int n)
{
  unsigned short (*q)[n], (*r)[n], (*s)[n], (*p)[n];
  q = (typeof (q)) qx;
  r = (typeof (r)) rx;
  s = (typeof (s)) sx;
  #pragma omp for
  for (p = q; p < r; p++)
    bar (p, 256);
  #pragma omp for
  for (p = s; p > r; p--)
    bar (p, 512);
  #pragma omp for
  for (p = q; p < r; p = p + 1)
    bar (p, 1024);
  #pragma omp for
  for (p = s; p > r; p = p - 1)
    bar (p, 2048);
  #pragma omp for
  for (p = q; p < r; p = 1 + p)
    bar (p, 4096);
  #pragma omp for
  for (p = s; p > r; p = -1 + p)
    bar (p, 8192);
  #pragma omp for
  for (p = q; p < r; p += 1)
    bar (p, 16384);
  #pragma omp for
  for (p = s; p > r; p -= 1)
    bar (p, 32768U);
}

int
main ()
{
  int i;
  volatile int j = 7;
#pragma omp parallel
  {
    foo (&a[5 + (j - 7)], &a[124 + (j - 7)], &a[222 + (j - 7)], j);
    baz (&a[5 + (j - 7)], &a[124 + (j - 7)], &a[222 + (j - 7)], j);
  }
  for (i = 0; i < 256; i++)
    if (i < 5 || i > 222 || i == 124 || ((i - 5) % 7) != 0)
      {
	if (a[i])
	  __builtin_abort ();
      }
    else if (i < 124 && a[i] != 1 + 4 + 16 + 64 + 256 + 1024 + 4096 + 16384)
      __builtin_abort ();
    else if (i > 124 && a[i] != 2 + 8 + 32 + 128 + 512 + 2048 + 8192 + 32768U)
      __builtin_abort ();
  return 0;
}
