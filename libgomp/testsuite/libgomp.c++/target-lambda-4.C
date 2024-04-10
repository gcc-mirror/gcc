int
foo ()
{
  int var = 42;
  [&var] () {
#pragma omp target firstprivate(var)
    {
      var += 26;
      if (var != 42 + 26)
	__builtin_abort ();
    }
  } ();
  return var;
}


template <typename T>
struct A {
  A () : a(), b()
  {
    [&] ()
    {
#pragma omp target firstprivate (a) map (from: b)
      b = ++a;
    } ();
  }

  T a, b;
};


int
main ()
{
  if (foo () != 42)
    __builtin_abort ();

  A<int> x;
  if (x.a != 0 || x.b != 1)
    __builtin_abort ();
}
