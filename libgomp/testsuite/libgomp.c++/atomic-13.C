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
  p = &x;
  #pragma omp atomic update
    p[foo (), 0] = 16 + 6 - p[foo (), 0];
  #pragma omp atomic read
    v = x;
  if (cnt != 2 || v != 16)
    abort ();
  #pragma omp atomic capture
    v = p[foo () + foo (), 0] = p[foo () + foo (), 0] + 3;
  if (cnt != 6 || v != 19)
    abort ();
  #pragma omp atomic capture
    v = p[foo (), 0] = 12 * 1 / 2 + (foo (), 0) + p[foo (), 0];
  if (cnt != 9 || v != 25)
    abort ();
  #pragma omp atomic capture
    {
      v = p[foo () & 0]; p[foo () & 0] = (foo (), 1) * 9 - p[foo () & 0];
    }
  if (cnt != 13 || v != 25)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != -16)
    abort ();
  #pragma omp atomic capture
    {
      p[0 & foo ()] = 16 - 2 + 3 + p[0 & foo ()]; v = p[0 & foo ()];
    }
  if (cnt != 16 || v != 1)
    abort ();
  #pragma omp atomic capture
    {
      v = p[foo (), 0]; p[foo (), 0] = (foo (), 7) ? 13 : foo () + 6;
    }
  if (cnt != 19 || v != 1)
    abort ();
  #pragma omp atomic read
    v = x;
  if (v != 13)
    abort ();
}

int x = 6;

int
main ()
{
  bar <int> ();
  return 0;
}
