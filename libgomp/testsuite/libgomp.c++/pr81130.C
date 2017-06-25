// PR c++/81130
// { dg-do run }

struct A
{
  A ();
  ~A ();
  int a;
};

A::A ()
{
  a = 0;
}

A::~A ()
{
}

struct B
{
  A b;
  int c;
  B () : c (1)
  {
#pragma omp parallel shared (b, c) num_threads (2)
#pragma omp master
    {
      b.a++;
      c += 2;
    }
  }
};

int
main ()
{
  B v;
  if (v.b.a != 1 || v.c != 3)
    __builtin_abort ();
}
