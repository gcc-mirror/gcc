// { dg-do run }

extern "C" void abort ();

struct S
{
  int s;
  S () : s (0) {}
  ~S () {}
};

#pragma omp declare reduction (+:S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:int:omp_out += omp_in)

int
main ()
{
  int i, u = 0, q = 0;
  S s, t;
  if (s.s != 0 || t.s != 0) abort ();
  #pragma omp parallel reduction(+:s, q) reduction(foo:t, u)
  {
    if (s.s != 0 || t.s != 0 || u != 0 || q != 0) abort ();
    s.s = 6;
    t.s = 8;
    u = 9;
    q++;
  }
  if (s.s != 6 * q || t.s != 8 * q || u != 9 * q) abort ();
  return 0;
}
