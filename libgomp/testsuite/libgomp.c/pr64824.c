/* PR c/64824 */
/* { dg-do run } */

int
main ()
{
  long long a;
  long long b = 1LL;
  int c = 3;
#pragma omp atomic capture
  a = b = c << b;
  if (b != 6LL || a != 6LL)
    __builtin_abort ();
  return 0;
}
