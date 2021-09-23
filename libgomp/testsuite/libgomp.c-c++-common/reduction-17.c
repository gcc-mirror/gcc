/* PR middle-end/99928 */
/* { dg-do run } */

#define N 64

int
main ()
{
  int r = 0, i;
  #pragma omp teams distribute simd reduction(+:r)
  for (i = 0; i < N; i++)
    r += i;
  if (r != N * (N - 1) / 2)
    __builtin_abort ();
  return 0;
}
