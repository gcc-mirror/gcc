#pragma omp declare target
    _Complex int *g;
#pragma omp end declare target



_Complex float f(void);

int
main ()
{
  _Complex int y;
#pragma omp target map(from:y)
  {
    _Complex int x;
    g = &x;
    __imag__ x = 1;
    __real__ x = 2;
    y = x;
  }

  if ((__imag__ y != 1)
      || (__real__ y != 2))
    __builtin_abort ();
  return 0;
}

