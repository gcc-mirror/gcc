// PR c++/127559
// { dg-do run }

template <typename T>
struct S {
  T x;
  int y;
};

template <typename T>
T
bar (T x)
{
  return x;
}

void
foo (int x, int y)
{
  auto b = [&] { return x; };
  S <decltype (b)> c = { b, 42 };
  S <decltype (b)> d[2] = { { b, 42 }, { b, 42 } };
  auto *p = &d[1];
  #pragma omp target
  if (c.x () != 42 || d[y].x () != 42 /* || bar (p)->x () != 42 */)
    __builtin_abort ();
}

int
main ()
{
  foo (42, 0);
}
