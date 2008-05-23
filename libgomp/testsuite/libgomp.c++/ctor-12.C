// PR c++/36308
// { dg-do run }

extern "C" void abort ();

static int n;

struct A
{
  A ()
  {
    l = 0;
    #pragma omp atomic
      ctors++;
  }
  A (const A &x)
  {
    l = x.l;
    #pragma omp atomic
      copyctors++;
  }
  virtual A& operator= (const A &x)
  {
    l = x.l;
    #pragma omp atomic
      assignops++;
    return *this;
  }
  virtual ~A ()
  {
    #pragma omp atomic
      dtors++;
  }
  int l;
  static int ctors, dtors, copyctors, assignops;
};

int A::ctors;
int A::dtors;
int A::copyctors;
int A::assignops;

int
main ()
{
  A a;
#pragma omp parallel private (a)
  {
    a.l = 6;
    #pragma omp single copyprivate (a)
    {
      a.l = 3;
    }
    if (a.l != 3)
      abort ();
    #pragma omp atomic
      n++;
  }
  if (A::ctors != n + 1
      || A::copyctors != 0
      || A::dtors != n
      || A::assignops != n - 1)
    abort ();
  return 0;
}
