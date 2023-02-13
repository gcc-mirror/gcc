// PR c++/108286
// { dg-do run }

struct S {
  int
  foo ()
  {
    int res = 0;
#pragma omp target map(size, ptr[:size], res) nowait
    res = ptr[size - 1];
#pragma omp taskwait
    return res;
  }

  unsigned size;
  int *ptr;
};

int
main ()
{
  S s;
  int buf[5];
  s.size = 5;
  s.ptr = buf;
  buf[4] = 42;
  if (s.foo () != 42)
    __builtin_abort ();
}
