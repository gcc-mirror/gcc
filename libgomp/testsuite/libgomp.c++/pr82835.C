// PR c++/82835
// { dg-do run }

int a, b;

template <class>
struct C {
  C (int x = a) : c (5) { if (x != 137) __builtin_abort (); }
  int c;
};

struct D {
  void foo ();
  int d;
};

void
D::foo ()
{
  C<int> c;
#pragma omp for private (c)
  for (b = 0; b < d; b++)
    c.c++;
}

int
main ()
{
  a = 137;
  D d;
  d.d = 16;
  d.foo ();
  return 0;
}
