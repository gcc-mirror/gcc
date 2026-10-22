// PR c++/127559
// { dg-do run }
// { dg-additional-options {-fdump-tree-original -fdump-tree-gimple} }

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

/* Lambda objects 'c.x', 'd[y].x' must be mapped, including their by-reference capture of 'x'.
   { dg-final { scan-tree-dump {map\(to:c\.x \[len: [0-9]+\]\) map\(tofrom:\*c\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:c\.x\.__x \[pointer assign, bias: 0\]\)} original } }
   { dg-final { scan-tree-dump {map\(to:d\[y\]\.x \[len: [0-9]+\]\) map\(tofrom:\*d\[y\]\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:d\[y\]\.x\.__x \[pointer assign, bias: 0\]\)} original } }
   { dg-final { scan-tree-dump {map\(struct:c \[len: 1\]\) map\(to:c\.x \[len: [0-9]+\]\) map\(tofrom:\*c\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:c\.x\.__x \[pointer assign, bias: 0\]\)} gimple } }
   { dg-final { scan-tree-dump {map\(struct_unord:d \[len: 1\]\) map\(to:d\[y\]\.x \[len: [0-9]+\]\) map\(tofrom:\*d\[y\]\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:d\[y\]\.x\.__x \[pointer assign, bias: 0\]\)} gimple } }
   ..., and in particular not as a whole, implicitly:
   { dg-final { scan-tree-dump-not {runtime_implicit} gimple } } */
