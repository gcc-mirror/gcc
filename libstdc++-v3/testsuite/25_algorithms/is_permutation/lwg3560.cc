// { dg-do run { target c++20 } }

// LWG 3560. ranges::equal and ranges::is_permutation should short-circuit
// for sized_ranges

#include <algorithm>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

struct X
{
  // Any equality comparison will cause the test to fail.
  bool operator==(const X&) const { VERIFY(false); }
};

void
test_std_is_permutation()
{
  X vals[3];
  __gnu_test::random_access_container<X> r1(vals, vals+3);
  __gnu_test::random_access_container<X> r2(vals, vals+2);
  VERIFY( ! std::is_permutation(r1.begin(), r1.end(), r2.begin(), r2.end()) );

  std::ranges::equal_to pred;
  VERIFY( ! std::is_permutation(r1.begin(), r1.end(), r2.begin(), r2.end(),
				pred) );
}

void
test_std_ranges_is_permutation()
{
  X vals[3];
  __gnu_test::test_random_access_range<X> r1(vals, vals+3);
  __gnu_test::test_random_access_range<X> r2(vals, vals+2);

  // Any application of the projection will cause the test to fail.
  auto proj = [](const X&) -> const X& { VERIFY(false); };
  VERIFY( ! std::ranges::is_permutation(r1.begin(), r1.end(),
					r2.begin(), r2.end(),
					{}, proj, proj) );

  __gnu_test::test_forward_sized_range<X> r3(vals, vals+3);
  __gnu_test::test_forward_sized_range<X> r4(vals, vals+2);
  VERIFY( ! std::ranges::is_permutation(r3, r4, {}, proj, proj) );
}

int main()
{
  test_std_is_permutation();
  test_std_ranges_is_permutation();
}
