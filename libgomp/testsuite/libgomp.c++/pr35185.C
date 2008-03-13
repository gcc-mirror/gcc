// PR middle-end/35185
// { dg-do run }

extern "C" void abort ();

struct S
{
  S () : s (6) {}
  ~S () {}
  int s;
};

__attribute__((noinline))
bool
bar (S s)
{
  return s.s != 6;
}

int
main ()
{
  S s;
  int err = 0;
#pragma omp parallel shared (s)
  {
    if (bar (s))
      #pragma omp atomic
	err++;
  }
  if (err)
    abort ();
}
