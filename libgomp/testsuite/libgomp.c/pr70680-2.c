/* PR middle-end/70680 */

int v;

void
f1 (void)
{
  int i = 0, j = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd collapse(2)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	;
    v = i + j;
  }
  if (i != 10 || j != 10)
    __builtin_abort ();
}

void
f2 (void)
{
  int i = 0, j = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd collapse(2)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	;
  }
  if (i != 10 || j != 10)
    __builtin_abort ();
}

void
f3 (void)
{
  int i = 0, j = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd collapse(2) lastprivate (i, j)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	;
    v = i + j;
  }
  if (i != 10 || j != 10)
    __builtin_abort ();
}

void
f4 (void)
{
  int i = 0, j = 0;
#pragma omp task default(shared) if(0)
  {
#pragma omp simd collapse(2) lastprivate (i, j)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
	;
  }
  if (i != 10 || j != 10)
    __builtin_abort ();
}

int
main ()
{
  f1 ();
  if (v++ != 20)
    __builtin_abort ();
  f2 ();
  f3 ();
  if (v++ != 20)
    __builtin_abort ();
  f4 ();
  return 0;
}
