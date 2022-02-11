
// We use 'auto' without a function return type, so specify dialect here
// { dg-additional-options "-std=c++14" }

extern "C" void abort ();

struct T
{
  int x, y;

  auto sum_func (int n)
  {
    auto fn = [=](int m) -> int
      {
	int v;
	v = (x + y) * n + m;
	return v;
      };
    return fn;
  }

  auto sum_func_offload (int n)
  {
    auto fn = [=](int m) -> int
      {
	int v;
	#pragma omp target map(from:v)
	v = (x + y) * n + m;
	return v;
      };
    return fn;
  }

};

int main (void)
{
  T a = { 1, 2 };

  auto s1 = a.sum_func (3);
  auto s2 = a.sum_func_offload (3);

  if (s1 (1) != s2 (1))
    abort ();

  return 0;
}
