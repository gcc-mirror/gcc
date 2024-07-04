/* { dg-do run } */

#define N 256

#pragma omp begin declare target indirect
int foo(void) { return 5; }
int bar(void) { return 8; }
int baz(void) { return 11; }
#pragma omp end declare target

int main (void)
{
  int i, x = 0, expected = 0;
  int (*fn_ptr[N])(void);

  for (i = 0; i < N; i++)
    {
      switch (i % 3)
	{
	case 0: fn_ptr[i] = &foo; break;
	case 1: fn_ptr[i] = &bar; break;
	case 2: fn_ptr[i] = &baz; break;
	}
      expected += (*fn_ptr[i]) ();
    }

  #pragma omp target teams distribute parallel for \
	reduction (+: x) map (to: fn_ptr) map (tofrom: x)
    for (int i = 0; i < N; i++)
      x += (*fn_ptr[i]) ();

  return x - expected;
}
