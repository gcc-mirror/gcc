// PR c++/88988
// { dg-do compile }
// { dg-additional-options "-std=c++14" }

extern "C" void abort ();

template <typename T>
struct A {
  A () : a(), b()
  {
    [&] ()
    {
#pragma omp task firstprivate (a) shared (b)
      b = ++a;
#pragma omp taskwait
    } ();
  }

  T a, b;
};

int
main ()
{
  A<int> x;
  if (x.a != 0 || x.b != 1)
    abort ();
}
