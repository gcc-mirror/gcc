extern "C" void abort ();
int a, b, c, d, e;

void
foo (int &x, bool y)
{
  #pragma omp task depend (out: x)
    a = 1;
  #pragma omp task depend (out: y ? b : c)
    (y ? b : c) = 2;
  #pragma omp task depend (inout: --d)
    d += 4;
  #pragma omp task depend (in : a, (y ? b : c), d)
    e = a + b * 10 + c * 100 + d * 1000;
}

int
main ()
{
  #pragma omp parallel
  #pragma omp single
  foo (a, true);
  if (e != 1 + 20 + 0 + 3000)
    abort ();
  a = b = c = d = e = 0;
  #pragma omp parallel
  #pragma omp single
  foo (a, false);
  if (e != 1 + 0 + 200 + 3000)
    abort ();
}
