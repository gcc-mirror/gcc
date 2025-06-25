// { dg-do run { target c++23 } }

#include <algorithm>
#include <ranges>

#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;

template<typename Range1, typename Range2>
void
test01()
{
  int n[] = {1,2,3,4,5,6,7,8,9,10};

  Range1 haystack(n, n+10);
  Range2 needle(n, n+3);
  VERIFY( ranges::starts_with(haystack, needle) );

  haystack = Range1(n);
  needle = Range2(n, n+10);
  VERIFY( ranges::starts_with(haystack, needle) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( !ranges::starts_with(haystack, needle) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( ranges::starts_with(haystack, needle,
			      [](int n, int m) { return std::abs(n - m) <= 1; }) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( ranges::starts_with(haystack, needle,
			      ranges::equal_to{},
			      [](int n) { return n + 1; }) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( ranges::starts_with(haystack, needle,
			      ranges::equal_to{},
			      std::identity{},
			      [](int n) { return n - 1; }) );

  haystack = Range1(n, n+5);
  needle = Range2(n, n+10);
  VERIFY( !ranges::starts_with(haystack, needle) );

  haystack = Range1(n, n+5);
  needle = Range2(n+10, n+10);
  VERIFY( ranges::starts_with(haystack, needle) );
}

template<typename Range1, typename Range2>
void
test02()
{
  int n[] = {1,2,3,4,5,6,7,8,9,10};

  Range1 haystack(n, n+10);
  Range2 needle(n, n+3);
  VERIFY( ranges::starts_with(haystack.begin(), haystack.end(),
			      needle.begin(), needle.end()) );

  haystack = Range1(n);
  needle = Range2(n, n+10);
  VERIFY( ranges::starts_with(haystack.begin(), haystack.end(),
			      needle.begin(), needle.end()) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( !ranges::starts_with(haystack.begin(), haystack.end(),
			       needle.begin(), needle.end()) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( ranges::starts_with(haystack.begin(), haystack.end(),
			      needle.begin(), needle.end(),
			      [](int n, int m) { return std::abs(n - m) <= 1; }) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( ranges::starts_with(haystack.begin(), haystack.end(),
			      needle.begin(), needle.end(),
			      ranges::equal_to{},
			      [](int n) { return n + 1; }) );

  haystack = Range1(n);
  needle = Range2(n+1, n+4);
  VERIFY( ranges::starts_with(haystack.begin(), haystack.end(),
			      needle.begin(), needle.end(),
			      ranges::equal_to{},
			      std::identity{},
			      [](int n) { return n - 1; }) );

  haystack = Range1(n, n+5);
  needle = Range2(n, n+10);
  VERIFY( !ranges::starts_with(haystack.begin(), haystack.end(),
			       needle.begin(), needle.end()) );
}

void
test03()
{
  auto haystack = std::views::iota(0, 10);
  auto needle = std::views::iota(0, 5);

#if __SIZEOF_INT128__
  auto haystack_ict = std::views::iota(__int128(0), __int128(10));
  auto needle_ict = std::views::iota(__int128(0), __int128(5));
#else
  auto haystack_ict = std::views::iota(0ll, 10ll);
  auto needle_ict = std::views::iota(0ll, 5ll);
#endif

  VERIFY( ranges::starts_with(haystack, needle_ict) );
  VERIFY( ranges::starts_with(haystack.begin(), haystack.end(),
			      needle_ict.begin(), needle_ict.end()) );

  VERIFY( ranges::starts_with(haystack_ict, needle) );
  VERIFY( ranges::starts_with(haystack_ict.begin(), haystack_ict.end(),
			      needle.begin(), needle.end()) );

  VERIFY( ranges::starts_with(haystack_ict, needle_ict) );
  VERIFY( ranges::starts_with(haystack_ict.begin(), haystack_ict.end(),
			      needle_ict.begin(), needle_ict.end()) );
}

int
main()
{
  using namespace __gnu_test;
  using input = test_input_range<int>;
  using input_sized = test_input_sized_range<int>;
  using input_sized_sent = test_sized_range_sized_sent<int, input_iterator_wrapper>;
  using random_access = test_random_access_range<int>;
  using random_access_sized = test_random_access_sized_range<int>;
  using random_access_sized_sent = test_sized_range_sized_sent<int, random_access_iterator_wrapper>;

  test01<input, input>();
  test01<random_access, random_access>();
  test02<input, input>();
  test02<random_access, random_access>();

  test01<input_sized, input_sized>();
  test01<random_access_sized, random_access_sized>();
  test02<input_sized, input_sized>();
  test02<random_access_sized, random_access_sized>();

  test01<input_sized_sent, input_sized_sent>();
  test01<random_access_sized_sent, random_access_sized_sent>();
  test02<input_sized_sent, input_sized_sent>();
  test02<random_access_sized_sent, random_access_sized_sent>();

  test03();
}
