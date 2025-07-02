// { dg-options "-std=gnu++98" }
// { dg-do compile { target c++98_only } }
// std::deque<character types>::resize() method fails with -std=c++98

#include <memory>
#include <testsuite_iterators.h>

void
test_pr120931()
{
  using __gnu_test::test_container;
  using __gnu_test::forward_iterator_wrapper;
  unsigned char c[1];
  test_container<unsigned char, forward_iterator_wrapper> f(c);
  std::uninitialized_fill(f.begin(), f.end(), 0);
}
