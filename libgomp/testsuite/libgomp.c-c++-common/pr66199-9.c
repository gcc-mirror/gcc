/* PR middle-end/66199 */
/* { dg-do run } */

#pragma omp declare target
int u[1024], v[1024], w[1024];
#pragma omp end declare target

__attribute__((noinline, noclone)) long
f2 (long a, long b, long c)
{
  long d, e;
  #pragma omp target map(from: d, e)
  #pragma omp teams default(none) firstprivate (a, b, c) shared(d, e, u, v, w)
  #pragma omp distribute lastprivate(d, e)
  for (d = a; d < b; d++)
    {
      u[d] = v[d] + w[d];
      e = c + d * 5;
    }
  return d + e;
}

__attribute__((noinline, noclone)) long
f3 (long a1, long b1, long a2, long b2)
{
  long d1, d2;
  #pragma omp target map(from: d1, d2)
  #pragma omp teams default(none) shared(a1, b1, a2, b2, d1, d2, u, v, w)
  #pragma omp distribute firstprivate (a1, b1, a2, b2) lastprivate(d1, d2) collapse(2)
  for (d1 = a1; d1 < b1; d1++)
    for (d2 = a2; d2 < b2; d2++)
      u[d1 * 32 + d2] = v[d1 * 32 + d2] + w[d1 * 32 + d2];
  return d1 + d2;
}

int
main ()
{
  if (f2 (0, 1024, 17) != 1024 + (17 + 5 * 1023)
      || f3 (0, 32, 0, 32) != 64)
    __builtin_abort ();
  return 0;
}
