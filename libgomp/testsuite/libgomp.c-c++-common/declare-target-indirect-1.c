/* { dg-do run } */

#pragma omp begin declare target indirect
int foo(void) { return 5; }
int bar(void) { return 8; }
int baz(void) { return 11; }
#pragma omp end declare target

int main (void)
{
  int x;
  int (*foo_ptr) (void) = &foo;
  int (*bar_ptr) (void) = &bar;
  int (*baz_ptr) (void) = &baz;
  int expected = foo () + bar () + baz ();

#pragma omp target map (to: foo_ptr, bar_ptr, baz_ptr) map (from: x)
  x = (*foo_ptr) () + (*bar_ptr) () + (*baz_ptr) ();

  return x - expected;
}
