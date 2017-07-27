/* PR c/45784 */
/* { dg-do run } */

void
foo (int n)
{
  char *p, vla[2 * n];
  int i;
  #pragma omp parallel for
  for (p = vla; p < vla + (sizeof (vla) / sizeof (vla[0])); p++)
    *p = ' ';
  #pragma omp parallel for
  for (i = 0; i < 2 * n; i++)
    if (vla[i] != ' ')
      __builtin_abort ();
}

void
bar (int n)
{
  char *p, vla1[n], vla2[n * 2], vla3[n * 3], vla4[n * 4];
  int i;
  __builtin_memset (vla4, ' ', n * 4);
  #pragma omp parallel for
  for (p = vla4 + sizeof (vla1); p < vla4 + sizeof (vla3) - sizeof (vla2) + sizeof (vla1); p += sizeof (vla4) / sizeof (vla4))
    p[0] = '!';
  #pragma omp parallel for
  for (i = 0; i < n * 4; i++)
    if (vla4[i] != ((i >= n && i < 2 * n) ? '!' : ' '))
      __builtin_abort ();
}

int
main ()
{
  volatile int n;
  n = 128;
  foo (n);
  bar (n);
  return 0;
}
