/* PR middle-end/66199 */
/* { dg-do run } */
/* { dg-options "-O2 -fopenmp" } */

int u[1024], v[1024], w[1024];

__attribute__((noinline, noclone)) long
f1 (long a, long b)
{
  long d;
  #pragma omp parallel for lastprivate (d) default(none) firstprivate (a, b) shared(u, v, w)
  for (d = a; d < b; d++)
    u[d] = v[d] + w[d];
  return d;
}

__attribute__((noinline, noclone)) long
f2 (long a, long b, long c)
{
  long d, e;
  #pragma omp parallel for lastprivate (d) default(none) firstprivate (a, b) shared(u, v, w) linear(c:5) lastprivate(e)
  for (d = a; d < b; d++)
    {
      u[d] = v[d] + w[d];
      c += 5;
      e = c;
    }
  return d + c + e;
}

__attribute__((noinline, noclone)) long
f3 (long a1, long b1, long a2, long b2)
{
  long d1, d2;
  #pragma omp parallel for default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) lastprivate(d1, d2) collapse(2)
  for (d1 = a1; d1 < b1; d1++)
    for (d2 = a2; d2 < b2; d2++)
      u[d1 * 32 + d2] = v[d1 * 32 + d2] + w[d1 * 32 + d2];
  return d1 + d2;
}

int
main ()
{
  if (f1 (0, 1024) != 1024
      || f2 (0, 1024, 17) != 1024 + 2 * (17 + 5 * 1024)
      || f3 (0, 32, 0, 32) != 64)
    __builtin_abort ();
  return 0;
}
