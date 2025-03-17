// { dg-do run { target c++23 } }

#include <algorithm>
#include <unordered_set>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

template<typename Range, typename V>
constexpr void
do_test()
{
  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,1,2,3,4,5};

  auto eq = [&](std::unordered_multiset<V> const& l,
		std::span<T> r) {
    if (l.size() != r.size())
      return false;

    return std::ranges::is_permutation(l, r);
  };

  std::unordered_multiset<V> s;
  s.insert_range(Range(a, a+0));
  VERIFY( s.empty() );

  s.insert_range(Range(a, a+4));
  VERIFY( eq(s, {a, 4}) );

  s.insert_range(Range(a+4, a+9));
  VERIFY( eq(s, {a, 9}) );

  s.insert_range(Range(a+9, a+14));
  VERIFY( eq(s, {a, 14}) );
}

template<typename Range>
void
do_test_v()
{
  do_test<Range, int>();
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_v<test_forward_range<int>>();
  do_test_v<test_range_nocopy<int, input_iterator_wrapper_nocopy>>();
  do_test_v<test_forward_range<short>>();

  return true;
}

int main()
{
  test_ranges();
}
