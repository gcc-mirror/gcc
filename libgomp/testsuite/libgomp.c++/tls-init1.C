// { dg-require-alias }

extern "C" void abort();

struct A
{
  A();
  int i;
};

extern A a;
#pragma omp threadprivate (a)
A a;

A &f()
{
  return a;
}

int j;
A::A(): i(j) { }

int main()
{
  j = 42;
  if (f().i != 42)
    abort ();
}
