// PR c++/127559
// { dg-do run }

void
foo (int *a)
{
  #pragma omp target map (to: a[0])
    {
      auto b = [=] { return *a; };
      auto c = [=] { return b (); };
      if (c () != 42)
	__builtin_abort ();
    }
}

int
main ()
{
  int a = 42;
  foo (&a);
}
