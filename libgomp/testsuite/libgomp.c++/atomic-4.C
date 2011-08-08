// { dg-do run }

extern "C" void abort (void);
template <typename T, typename T2>
int
foo (void)
{
  extern T x;
  extern T2 y;
  T v;
  T2 f;
  #pragma omp atomic read
    v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic write
    x = 17;
  #pragma omp atomic read
  v = x;
  if (v != 17)
    abort ();
  #pragma omp atomic update
    x++;
  #pragma omp atomic read
    v = x;
  if (v != 18)
    abort ();
  #pragma omp atomic capture
    v = x++;
  if (v != 18)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 19)
    abort ();
  #pragma omp atomic capture
    v = ++x;
  if (v != 20)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 20)
    abort ();
  #pragma omp atomic capture
    { v = x; x *= 3; }
  if (v != 20)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 60)
    abort ();
  #pragma omp atomic capture
    {
      x |= 2;
      v = x;
    }
  if (v != 62)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 62)
    abort ();
  #pragma omp atomic capture
    { v = x; x++; }
  if (v != 62)
    abort ();
  #pragma omp atomic capture
    { v = x; ++x; }
  if (v != 63)
    abort ();
  #pragma omp atomic capture
    {
      ++x;
      v = x;
    }
  if (v != 65)
    abort ();
#pragma omp atomic capture
{x++;v=x;}if (v != 66)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 66)
    abort ();
  #pragma omp atomic capture
    { v = x; x--; }
  if (v != 66)
    abort ();
  #pragma omp atomic capture
    { v = x; --x; }
  if (v != 65)
    abort ();
  #pragma omp atomic capture
    {
      --x;
      v = x;
    }
  if (v != 63)
    abort ();
  #pragma omp atomic capture
  { x--; v = x; } if (v != 62)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 62)
    abort ();
  #pragma omp atomic write
    y = 17.5f;
  #pragma omp atomic read
    f = y;
  if (f != 17.5)
    abort ();
  #pragma omp atomic update
    y *= 2.0f;
  #pragma omp atomic read
    f = y;
  if (y != 35.0)
    abort ();
  #pragma omp atomic capture
    f = y *= 2.0f;
  if (f != 70.0)
    abort ();
  #pragma omp atomic capture
    f = y++;
  if (f != 70.0)
    abort ();
  #pragma omp atomic read
    f = y;
  if (f != 71.0)
    abort ();
  #pragma omp atomic capture
    f = --y;
  if (f != 70.0)
    abort ();
  #pragma omp atomic read
    f = y;
  if (f != 70.0)
    abort ();
  #pragma omp atomic capture
    { f = y; y /= 2.0f; }
  if (f != 70.0)
    abort ();
  #pragma omp atomic read
    f = y;
  if (f != 35.0)
    abort ();
  #pragma omp atomic capture
    { y /= 2.0f; f = y; }
  if (f != 17.5)
    abort ();
  #pragma omp atomic read
    f = y;
  if (f != 17.5)
    abort ();
  return 0;
}

int x = 6;
float y;

int
main ()
{
  foo <int, float> ();
  return 0;
}
