// { dg-do run }

extern "C" void abort (void);
long long l, m;
int i, j;

void
foo (void)
{
  #pragma omp atomic read
    i = l;
  #pragma omp atomic read
    m = j;
  if (i != 77 || m != 88)
    abort ();
  #pragma omp atomic write
    l = 1 + i + 6 * 1;
  #pragma omp atomic write
    j = 170 - 170 + m + 1 * 7;
  #pragma omp atomic capture
    i = l += 4;
  #pragma omp atomic capture
    m = j += 4;
  if (i != 88 || m != 99)
    abort ();
  #pragma omp atomic capture
    {
      i = l;
      l += 4;
    }
  #pragma omp atomic capture
    {
      m = j;
      j += 4;
    }
  if (i != 88 || m != 99)
    abort ();
  #pragma omp atomic capture
    {
      l += 4;
      i = l;
    }
  #pragma omp atomic capture
    {
      j += 4;
      m = j;
    }
  if (i != 96 || m != 107)
    abort ();
}

int
main ()
{
  l = 77;
  j = 88;
  foo ();
}
