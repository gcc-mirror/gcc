/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=4 -ffast-math" } */

extern void abort (void);

double d[1024], e[1024];
int f[1024], g[1024];

double __attribute__((noinline))
foo (void)
{
  double s = 0.0;
  int i;
  for (i = 0; i < 1024; i++)
    s += d[i] - e[i];
  return s;
}

int __attribute__((noinline))
bar (void)
{
  int s = 0, i;
  for (i = 0; i < 1024; i++)
    s += f[i] - g[i];
  return s;
}

int
main (void)
{
  int i;
  for (i = 0; i < 1024; i++)
    {
      d[i] = i * 2;
      e[i] = i;
      f[i] = i * 2;
      g[i] = i;
    }
  if (foo () != 1023 * 1024 / 2)
    abort ();
  if (bar () != 1023 * 1024 / 2)
    abort ();
  return 0;
}
