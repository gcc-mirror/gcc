/* PR middle-end/70680 */

int v;

void
f1 (void)
{
  int i = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd
    for (i = 0; i < 100; i++)
      ;
    v = i;
  }
  if (i != 100)
    __builtin_abort ();
}

void
f2 (void)
{
  int i = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd
    for (i = 0; i < 100; i++)
      ;
  }
  if (i != 100)
    __builtin_abort ();
}

void
f3 (void)
{
  int i = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd linear(i: 1)
    for (i = 0; i < 100; i++)
      ;
    v = i;
  }
  if (i != 100)
    __builtin_abort ();
}

void
f4 (void)
{
  int i = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd linear(i: 1)
    for (i = 0; i < 100; i++)
      ;
  }
  if (i != 100)
    __builtin_abort ();
}

int
main ()
{
  f1 ();
  if (v++ != 100)
    __builtin_abort ();
  f2 ();
  f3 ();
  if (v++ != 100)
    __builtin_abort ();
  f4 ();
  return 0;
}
