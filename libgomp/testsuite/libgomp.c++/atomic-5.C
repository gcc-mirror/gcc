// { dg-do run }

extern "C" void abort (void);

template <typename T>
void
foo ()
{
  extern T v, x1, x2, x3, x4, x5, x6;
  #pragma omp atomic capture
  v = ++x1;
  if (!v)
    abort ();
  #pragma omp atomic capture
  v = x2++;
  if (v)
    abort ();
  #pragma omp atomic read
  v = x3;
  if (!v)
    abort ();
  #pragma omp atomic read
  v = x4;
  if (!v)
    abort ();
  #pragma omp atomic capture
  { v = x5; x5 |= 1; }
  if (v)
    abort ();
  #pragma omp atomic capture
  { x6 |= 1; v = x6; }
  if (!v)
    abort ();
}

template <typename T>
void
bar ()
{
  extern T v, x1, x2;
  #pragma omp atomic write
  x1 = false;
  #pragma omp atomic write
  x2 = false;
  #pragma omp atomic capture
  { ++x1; v = x1; }
  if (!v)
    abort ();
  #pragma omp atomic capture
  { v = x2; x2++; }
  if (v)
    abort ();
  #pragma omp atomic write
  x1 = false;
  #pragma omp atomic write
  x2 = false;
  #pragma omp atomic capture
  { x1++; v = x1; }
  if (!v)
    abort ();
  #pragma omp atomic capture
  { v = x2; ++x2; }
  if (v)
    abort ();
}

bool v, x1, x2, x3, x4, x5, x6;

int
main ()
{
  #pragma omp atomic write
  x3 = true;
  #pragma omp atomic write
  x4 = true;
  foo <bool> ();
  bar <bool> ();
  return 0;
}
