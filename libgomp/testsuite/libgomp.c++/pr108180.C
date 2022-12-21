// PR c++/108180
// { dg-do run }

struct A {
  A () { ++a; }
  A (A &&) = delete;
  A (const A &) { ++a; }
  A &operator= (const A &) = delete;
  A &operator= (A &&) = delete;
  ~A () { --a; }
  static int a;
};
int A::a = 0;

struct B {
  A a;
  template <int N>
  int
  foo ()
  {
    int res = 0;
    #pragma omp parallel for if(false) firstprivate(a)
    for (int i = 0; i < 64; ++i)
      res += i;
    return res;
  }
  int
  bar ()
  {
    int res = 0;
    #pragma omp parallel for if(false) firstprivate(a)
    for (int i = 0; i < 64; ++i)
      res += i;
    return res;
  }
};

int
main ()
{
  {
    B b;
    if (b.foo<0> () != 2016)
      __builtin_abort ();
  }
  if (A::a != 0)
    __builtin_abort ();
  {
    B b;
    if (b.bar () != 2016)
      __builtin_abort ();
  }
  if (A::a != 0)
    __builtin_abort ();
}
