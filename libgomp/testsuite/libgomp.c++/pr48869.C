// PR c++/48869
// { dg-do run }
// { dg-options "-std=gnu++0x" }

template <const int N>
struct A
{
  A () {}
  A (const A&) = delete;
  void foo () {}
  ~A () {}
};

template <const int N>
struct B
{
  B () {}
  B (const B&) {}
  void foo () {}
  ~B () {}
};

void __attribute__((used))
foo (B<6> b6)
{
  #pragma omp task
    b6.foo ();
}

int
main ()
{
  A<0> a0;
  #pragma omp task shared(a0)
    a0.foo ();
  #pragma omp task default(shared)
    a0.foo ();
  #pragma omp parallel shared(a0)
    #pragma omp task
      a0.foo ();
  #pragma omp task
  {
    A<1> a1;
    a1.foo ();
  }
  B<0> b0;
  #pragma omp task shared(b0)
    b0.foo ();
  B<1> b1;
  #pragma omp task default(shared)
    b1.foo ();
  B<2> b2;
  #pragma omp parallel shared(b2)
    #pragma omp task
      b2.foo ();
  B<3> b3;
  #pragma omp task
    b3.foo ();
  B<4> b4;
  #pragma omp parallel private (b4)
    #pragma omp task
      b4.foo ();
  B<5> b5;
  #pragma omp parallel firstprivate (b5)
    #pragma omp task
      b5.foo ();
  return 0;
}
