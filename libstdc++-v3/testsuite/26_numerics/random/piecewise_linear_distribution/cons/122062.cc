// { dg-do compile { target c++11 } }

// PR libstdc++/122062
// piecewise_linear_distribution(firstB, lastB, firstW) invokes comma operator

#include <random>
#include <testsuite_iterators.h>

void
test_pr122062()
{
  double b[1]{};
  double w[1]{};
  __gnu_test::random_access_container<double> B(b), W(w);
  std::piecewise_linear_distribution<double> p(B.begin(), B.end(), W.begin());
}
