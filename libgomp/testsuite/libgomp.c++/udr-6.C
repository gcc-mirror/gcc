// { dg-do run }

extern "C" void abort ();

struct A { int a; A () : a (6) {} };
struct B { int b; B () : b (5) {} };
struct C { int c; C () : c (4) {} };
struct D { int d; D () : d (3) {} };
struct E : A, B {};
struct F : C, D {};
struct G : E, F {};
void foo (B &);
void foo (F &);
#pragma omp declare reduction (+:B:omp_out.b += omp_in.b) \
		    initializer(foo (omp_priv))

void
foo (B &x)
{
  if (x.b != 5)
    abort ();
  x.b = 9;
}

template <typename T>
void bar (T &x, T &y, int z)
{
  if (z)
    abort ();
  x.a += y.a;
}

namespace N1
{
  struct A { int a; A () : a (0) {} };
  #pragma omp declare reduction (+:A:bar (omp_out, omp_in, 0))
};
namespace N2
{
  struct B : N1::A { };
  #pragma omp declare reduction (+:N1::A:bar (omp_out, omp_in, 1))
};

int
main ()
{
  G g;
  int i = 0;
  #pragma omp parallel reduction(+:g, i)
    {
      if (g.a != 6 || g.b != 9 || g.c != 4 || g.d != 3)
	abort ();
      g.a = 1, g.b = 2, g.c = 3, g.d = 4, i = 1;
    }
  if (g.a != 6 || g.b != 5 + 2 * i || g.c != 4 || g.d != 3)
    abort ();
  N2::B b;
  i = 0;
  #pragma omp parallel reduction (+:b, i)
    {
      if (b.a != 0)
	abort ();
      b.a = 4;
      i = 1;
    }
  if (b.a != 4 * i)
    abort ();
}
