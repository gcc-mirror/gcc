// { dg-do run }

#pragma omp begin declare target indirect
class C
{
public:
  int y;
  int f (int x) { return x + y; }
};
#pragma omp end declare target

int main (void)
{
  C c;
  c.y = 27;
  int x;
  int (C::*fn_ptr) (int) = &C::f;

#pragma omp target map (to: c, fn_ptr) map (from: x)
  x = (c.*fn_ptr) (42);

  return x != 27 + 42;
}
