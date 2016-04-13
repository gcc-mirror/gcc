// PR c++/70376
// { dg-do link }

template <typename T>
struct A
{
  A() { }
  A(const A&) { }
  void foo() { }
};

int
main ()
{
  A<int> a;
  #pragma omp taskloop
  for (int i = 0; i < 64; i++)
    a.foo();
  return 0;
}
