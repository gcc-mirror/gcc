extern void abort (void);
int a[16], b[16], c[16], d[5][2];

__attribute__((noinline, noclone)) void
foo (int x, int y)
{
  int i;
  #pragma omp for schedule (static, 1) reduction (+:a[:3])
  for (i = 0; i < 64; i++)
    {
      a[0] += i;
      a[1] += 2 * i;
      a[2] += 3 * i;
    }
  #pragma omp for schedule (guided) reduction (+:b[4:3])
  for (i = 0; i < 64; i++)
    {
      b[4] += i;
      b[5] += 2 * i;
      b[6] += 3 * i;
    }
  #pragma omp for schedule (static) reduction (+:c[x:4])
  for (i = 0; i < 64; i++)
    {
      c[9] += i;
      c[10] += 2 * i;
      c[11] += 3 * i;
      c[12] += 4 * i;
    }
  #pragma omp for reduction (+:d[x - 8:2][y:])
  for (i = 0; i < 64; i++)
    {
      d[1][0] += i;
      d[1][1] += 2 * i;
      d[2][0] += 3 * i;
      d[2][1] += 4 * i;
    }
}

int
main ()
{
  int i;
  #pragma omp parallel
  foo (9, 0);
  for (i = 0; i < 16; i++)
    if (a[i] != (i < 3 ? 64 * 63 / 2 * (i + 1) : 0)
	|| b[i] != ((i >= 4 && i < 7) ? 64 * 63 / 2 * (i - 3) : 0)
	|| c[i] != ((i >= 9 && i < 13) ? 64 * 63 / 2 * (i - 8) : 0))
      abort ();
  for (i = 0; i < 5; i++)
    if (d[i][0] != ((i && i <= 2) ? 64 * 63 / 2 * (2 * i - 1) : 0)
	|| d[i][1] != ((i && i <= 2) ? 64 * 63 / 2 * (2 * i) : 0))
      abort ();
  return 0;
}
