/* PR middle-end/94366 */

int
main ()
{
  int a = 2;
  #pragma omp parallel reduction(&& : a)
    a = a && 1;
  if (!a)
    __builtin_abort ();
  a = 4;
  #pragma omp parallel reduction(|| : a)
    a = a || 0;
  if (!a)
    __builtin_abort ();
  return 0;
}
