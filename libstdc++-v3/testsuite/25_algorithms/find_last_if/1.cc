// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;

template<int N> constexpr auto eq = [](int m) { return m == N; };

constexpr bool
test01()
{
  int x[] = {1, 2, 1, 2, 1, 2, 1, 2};

  auto sr0 = ranges::find_last_if(x, eq<0>);
  VERIFY( ranges::empty(sr0) );
  VERIFY( sr0.begin() == ranges::end(x) );

  auto sr1 = ranges::find_last_if(x, eq<1>);
  VERIFY( ranges::equal(sr1, (int[]){1, 2}) );
  VERIFY( sr1.begin() == &x[6] );

  auto sr2 = ranges::find_last_if(x, eq<2>);
  VERIFY( ranges::equal(sr2, (int[]){2}) );
  VERIFY( sr2.begin() == &x[7] );

  auto plus3 = [](int n) { return n+3; };

  auto sr3 = ranges::find_last_if(x, eq<3>, plus3);
  VERIFY( ranges::empty(sr3) );
  VERIFY( sr3.begin() == ranges::end(x) );

  auto sr4 = ranges::find_last_if(x, eq<4>, plus3);
  VERIFY( ranges::equal(sr4, (int[]){1, 2}) );
  VERIFY( sr4.begin() == &x[6] );

  auto sr5 = ranges::find_last_if(x, eq<5>, plus3);
  VERIFY( ranges::equal(sr5, (int[]){2}) );
  VERIFY( sr5.begin() == &x[7] );

  return true;
}

void
test02()
{
  int x[] = {1, 2, 3, 1, 2, 3, 1, 2, 3};
  __gnu_test::test_forward_range<int> rx(x);

  auto sr0 = ranges::find_last_if(rx, eq<0>);
  VERIFY( ranges::empty(sr0) );
  VERIFY( sr0.begin() == ranges::end(rx) );

  auto sr1 = ranges::find_last_if(rx, eq<1>);
  VERIFY( ranges::equal(sr1, (int[]){1, 2, 3}) );
  VERIFY( sr1.begin().ptr == &x[6] );

  auto sr2 = ranges::find_last_if(rx, eq<2>);
  VERIFY( ranges::equal(sr2, (int[]){2, 3}) );
  VERIFY( sr2.begin().ptr == &x[7] );

  auto sr3 = ranges::find_last_if(rx, eq<3>);
  VERIFY( ranges::equal(sr3, (int[]){3}) );
  VERIFY( sr3.begin().ptr == &x[8] );

  auto plus4 = [](int n) { return n+4; };

  auto sr4 = ranges::find_last_if(rx, eq<4>, plus4);
  VERIFY( ranges::empty(sr4) );
  VERIFY( sr4.begin() == ranges::end(rx) );

  auto sr5 = ranges::find_last_if(rx, eq<5>, plus4);
  VERIFY( ranges::equal(sr5, (int[]){1, 2, 3}) );
  VERIFY( sr5.begin().ptr == &x[6] );

  auto sr6 = ranges::find_last_if(rx, eq<6>, plus4);
  VERIFY( ranges::equal(sr6, (int[]){2, 3}) );
  VERIFY( sr6.begin().ptr == &x[7] );

  auto sr7 = ranges::find_last_if(rx, eq<7>, plus4);
  VERIFY( ranges::equal(sr7, (int[]){3}) );
  VERIFY( sr7.begin().ptr == &x[8] );
}

int
main()
{
  static_assert(test01());
  test02();
}
