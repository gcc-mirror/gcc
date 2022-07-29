/* PR middle-end/106449 */
/* { dg-do run } */

void
foo (void)
{
  int a[1024], *b[65536];
  int *p, *q, **r = &b[0], i;
  #pragma omp simd collapse(2) linear(r : 2)
  for (p = &a[0]; p < &a[512]; p++)
    for (q = p + 64; q < p + 128; q++)
      {
        *r++ = p;
        *r++ = q;
      }
  for (i = 0; i < 32768; i++)
    if (b[2 * i] != &a[i / 64] || b[2 * i + 1] != &a[(i / 64) + 64 + (i % 64)])
      __builtin_abort ();
}

void
bar (int n, int m)
{
  int a[1024], *b[65536];
  int *p, *q, **r = &b[0], i;
  #pragma omp parallel for simd collapse(2) linear(r : 2)
  for (p = &a[0]; p < &a[512]; p++)
    for (q = p + n; q < p + m; q++)
      {
        *r++ = p;
        *r++ = q;
      }
  for (i = 0; i < 32768; i++)
    if (b[2 * i] != &a[i / 64] || b[2 * i + 1] != &a[(i / 64) + 64 + (i % 64)])
      __builtin_abort ();
}

void
baz (int n, int m)
{
  int a[1024], *b[8192];
  int *p, *q, **r = &b[0], i;
  #pragma omp parallel for simd collapse(2) linear(r : 2)
  for (p = &a[0]; p < &a[512]; p += 4)
    for (q = p + n; q < p + m; q += 2)
      {
        *r++ = p;
        *r++ = q;
      }
  for (i = 0; i < 4096; i++)
    if (b[2 * i] != &a[(i / 32) * 4] || b[2 * i + 1] != &a[(i / 32) * 4 + 64 + (i % 32) * 2])
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
