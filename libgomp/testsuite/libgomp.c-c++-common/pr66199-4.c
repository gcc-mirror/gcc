/* PR middle-end/66199 */
/* { dg-do run } */
/* { dg-options "-O2" { target c } } */

#pragma omp declare target
int u[1024], v[1024], w[1024];
#pragma omp end declare target

__attribute__((noinline, noclone)) void
f1 (long a, long b)
{
  long d;
  #pragma omp target teams distribute parallel for default(none) firstprivate (a, b) shared(u, v, w)
  for (d = a; d < b; d++)
    u[d] = v[d] + w[d];
}

__attribute__((noinline, noclone)) void
f2 (long a, long b, long c)
{
  long d, e;
  #pragma omp target teams distribute parallel for default(none) firstprivate (a, b, c) shared(u, v, w) lastprivate(d, e)
  for (d = a; d < b; d++)
    {
      u[d] = v[d] + w[d];
      e = c + d * 5;
    }
}

__attribute__((noinline, noclone)) void
f3 (long a1, long b1, long a2, long b2)
{
  long d1, d2;
  #pragma omp target teams distribute parallel for default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) lastprivate(d1, d2) collapse(2)
  for (d1 = a1; d1 < b1; d1++)
    for (d2 = a2; d2 < b2; d2++)
      u[d1 * 32 + d2] = v[d1 * 32 + d2] + w[d1 * 32 + d2];
}

__attribute__((noinline, noclone)) void
f4 (long a1, long b1, long a2, long b2)
{
  long d1, d2;
  #pragma omp target teams distribute parallel for default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) collapse(2)
  for (d1 = a1; d1 < b1; d1++)
    for (d2 = a2; d2 < b2; d2++)
      u[d1 * 32 + d2] = v[d1 * 32 + d2] + w[d1 * 32 + d2];
}

int
main ()
{
  f1 (0, 1024);
  f2 (0, 1024, 17);
  f3 (0, 32, 0, 32);
  f4 (0, 32, 0, 32);
  return 0;
}
