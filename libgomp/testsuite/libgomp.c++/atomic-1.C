// PR c++/33894
// { dg-do run }

extern "C" void abort ();

int check;

template<typename T> void
foo ()
{
  #pragma omp atomic
  check |= sizeof (T);
}

template<typename T> void
bar (T *x, T y)
{
  #pragma omp atomic
  *x += y;
}

template<typename T> void
baz ()
{
  #pragma omp atomic
  check++;
}

int
main ()
{
  int i = 0;
  long l = 0;

  check = 0;
  foo<char> ();
  if (check != sizeof (char))
    abort ();
  foo<short> ();
  if (check != (sizeof (char) | sizeof (short)))
    abort ();
  bar(&i, 4);
  bar(&l, 8L);
  if (i != 4 || l != 8L)
    abort ();
  baz<char> ();
  if (check != (sizeof (char) | sizeof (short)) + 1)
    abort ();
  baz<long double> ();
  if (check != (sizeof (char) | sizeof (short)) + 2)
    abort ();
}
