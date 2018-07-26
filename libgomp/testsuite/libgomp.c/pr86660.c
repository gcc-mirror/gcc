/* PR middle-end/86660 */

#pragma omp declare target
int v[20];

void
foo (void)
{
  if (v[7] != 2)
    __builtin_abort ();
  v[7] = 1;
}
#pragma omp end declare target

int
main ()
{
  v[5] = 8;
  v[7] = 2;
  #pragma omp target map (always, tofrom: v)
  {
    foo ();
    v[5] = 3;
  }
  if (v[7] != 1 || v[5] != 3)
    __builtin_abort ();
  return 0;
}
