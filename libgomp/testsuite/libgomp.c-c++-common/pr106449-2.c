/* { dg-do run } */

/* Based on pr106449.c - but using 'for' instead of 'simd'.
   Cf. PR middle-end/106449 (for pr106449.c) and PR middle-end/106467.  */

void
foo (void)
{
  int a[1024], *b[65536], *c[65536];
  int *p, *q, **r = &b[0], **r2 = &c[0], i;
  #pragma omp for collapse(2)
  for (p = &a[0]; p < &a[512]; p++)
    for (q = p + 64; q < p + 128; q++)
      {
	*r++ = p;
	*r2++ = q;
      }
  for (i = 0; i < 32768; i++)
    if (b[i] != &a[i / 64] || c[i] != &a[(i / 64) + 64 + (i % 64)])
      __builtin_abort ();
}

void
bar (int n, int m)
{
  int a[1024], *b[32768], *c[32768];
  int *p, *q, **r = &b[0], **r2 = &c[0], i;
  #pragma omp for collapse(2)
  for (p = &a[0]; p < &a[512]; p++)
    for (q = p + n; q < p + m; q++)
      {
	*r++ = p;
	*r2++ = q;
      }
  for (i = 0; i < 32768; i++)
    if (b[i] != &a[i / 64] || c[i] != &a[(i / 64) + 64 + (i % 64)])
      __builtin_abort ();
}

void
baz (int n, int m)
{
  int a[1024], *b[8192], *c[8192];
  int *p, *q, **r = &b[0], **r2 = &c[0], i;
  #pragma omp for collapse(2)
  for (p = &a[0]; p < &a[512]; p += 4)
    for (q = p + n; q < p + m; q += 2)
      {
	*r++ = p;
	*r2++ = q;
      }
  for (i = 0; i < 4096; i++)
    if (b[i] != &a[(i / 32) * 4] || c[i] != &a[(i / 32) * 4 + 64 + (i % 32) * 2])
      __builtin_abort ();
}

int
main ()
{
  foo ();
  bar (64, 128);
  baz (64, 128);
  return 0;
}
