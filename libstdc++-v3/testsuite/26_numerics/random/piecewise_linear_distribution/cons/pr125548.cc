// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

#include <random>
#include <testsuite_hooks.h>

void
test_functor(int n)
{
  const double step = 1.0/n;
#if _GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS
  double expected = 0.0;
#else
  double expected = step;
#endif

  auto check_val = [&] (double value) mutable {
    VERIFY( value == expected );
    expected += step;
    return value;
  };

  std::piecewise_linear_distribution<> d(n, 0.0, 1.0, check_val);
}

int
main()
{
  test_functor(1);
  test_functor(2);
  test_functor(4);
  test_functor(8);
}
