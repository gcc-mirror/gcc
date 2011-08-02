// { dg-do run }

extern "C" void abort ();

int cnt;

int
foo ()
{
  return cnt++;
}

template <typename T>
void
bar ()
{
  extern T x;
  T v, *p;
  #pragma omp atomic update
    x = x + 7;
  #pragma omp atomic
    x = x + 7 + 6;
  #pragma omp atomic update
    x = x + 2 * 3;
  #pragma omp atomic
    x = x * (2 - 1);
  #pragma omp atomic read
    v = x;
  if (v != 32)
    abort ();
  #pragma omp atomic write
    x = 0;
  #pragma omp atomic capture
    {
      v = x;
      x = x | 1 ^ 2;
    }
  if (v != 0)
    abort ();
  #pragma omp atomic capture
    {
      v = x;
      x = x | 4 | 2;
    }
  if (v != 3)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 7)
    abort ();
  #pragma omp atomic capture
    {
      x = x ^ 6 & 2;
      v = x;
    }
  if (v != 5)
    abort ();
  #pragma omp atomic capture
    { x = x - (6 + 4); v = x; }
  if (v != -5)
    abort ();
  #pragma omp atomic capture
    { v = x; x = x - (1 | 2); }
  if (v != -5)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != -8)
    abort ();
  #pragma omp atomic
    x = x * -4 / 2;
  #pragma omp atomic read
    v = x;
  if (v != 16)
    abort ();
  p = &x;
  #pragma omp atomic update
    p[foo (), 0] = p[foo (), 0] - 16;
  #pragma omp atomic read
    v = x;
  if (cnt != 2 || v != 0)
    abort ();
  #pragma omp atomic capture
    {
      p[foo (), 0] += 6;
      v = p[foo (), 0];
    }
  if (cnt != 4 || v != 6)
    abort ();
  #pragma omp atomic capture
    {
      v = p[foo (), 0];
      p[foo (), 0] += 6;
    }
  if (cnt != 6 || v != 6)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic capture
    {
      p[foo (), 0] = p[foo (), 0] + 6;
      v = p[foo (), 0];
    }
  if (cnt != 9 || v != 18)
    abort ();
  #pragma omp atomic capture
    {
      v = p[foo (), 0];
      p[foo (), 0] = p[foo (), 0] + 6;
    }
  if (cnt != 12 || v != 18)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 24)
    abort ();
  #pragma omp atomic capture
  { v = p[foo (), 0]; p[foo (), 0]++; }
  #pragma omp atomic capture
  { v = p[foo (), 0]; ++p[foo (), 0]; }
  #pragma omp atomic capture
  { p[foo (), 0]++; v = p[foo (), 0]; }
  #pragma omp atomic capture
  { ++p[foo (), 0]; v = p[foo (), 0]; }
  if (cnt != 20 || v != 28)
    abort ();
  #pragma omp atomic capture
  { v = p[foo (), 0]; p[foo (), 0]--; }
  #pragma omp atomic capture
  { v = p[foo (), 0]; --p[foo (), 0]; }
  #pragma omp atomic capture
  { p[foo (), 0]--; v = p[foo (), 0]; }
  #pragma omp atomic capture
  { --p[foo (), 0]; v = p[foo (), 0]; }
  if (cnt != 28 || v != 24)
    abort ();
}

int x = 6;

int
main ()
{
  bar <int> ();
  return 0;
}
