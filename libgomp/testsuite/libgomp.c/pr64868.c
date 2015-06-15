/* PR c/64868 */
/* { dg-do run } */

float f = 2.0f;
double d = 4.0;
long double ld = 8.0L;

void
foo ()
{
#pragma omp atomic
  f = 1.0f / f;
#pragma omp atomic
  f = 1 / f;
#pragma omp atomic
  f = f / 2.0f;
#pragma omp atomic
  f = f / 2;
#pragma omp atomic
  f /= 2.0f;
#pragma omp atomic
  f /= 2;
#pragma omp atomic
  d = 1.0 / d;
#pragma omp atomic
  d = 1 / d;
#pragma omp atomic
  d = d / 2.0;
#pragma omp atomic
  d = d / 2;
#pragma omp atomic
  d /= 2.0;
#pragma omp atomic
  d /= 2;
#pragma omp atomic
  ld = 1.0L / ld;
#pragma omp atomic
  ld = 1 / ld;
#pragma omp atomic
  ld = ld / 2.0L;
#pragma omp atomic
  ld = ld / 2;
#pragma omp atomic
  ld /= 2.0L;
#pragma omp atomic
  ld /= 2;
  if (f != 0.125f || d != 0.25 || ld != 0.5L)
    __builtin_abort ();
}

#ifdef __cplusplus
template <typename T, int N1, int N2>
void
bar ()
{
  T v = ::d;
#pragma omp atomic
  v *= 16;
#pragma omp atomic
  v = 1.0 / v;
#pragma omp atomic
  v = N1 / v;
#pragma omp atomic
  v = v / 2.0;
#pragma omp atomic
  v = v / N2;
#pragma omp atomic
  v /= 2.0;
#pragma omp atomic
  v /= N2;
  if (v != 0.25)
    __builtin_abort ();
}
#endif

int
main ()
{
  foo ();
#ifdef __cplusplus
  bar<float, 1, 2> ();
  bar<double, 1, 2> ();
  bar<long double, 1, 2> ();
#endif
  return 0;
}
