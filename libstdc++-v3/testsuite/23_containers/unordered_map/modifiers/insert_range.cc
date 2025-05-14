// { dg-do run { target c++23 } }

#include <algorithm>
#include <unordered_map>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

template<typename Range, typename K, typename V>
constexpr void
do_test()
{
  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{{1,2},{2,3},{3,4},{4,5},{5,6},{6,7},{7,8},{8,9},{9,0},
	{1,1},{2,2},{3,3},{4,4},{5,5}};

  auto eq = [&](std::unordered_map<K, V> const& l,
		std::span<T> r) {
    if (l.size() != r.size())
      return false;

    return std::ranges::is_permutation(l, r);
  };

  std::unordered_map<K, V> m;
  m.insert_range(Range(a, a+0));
  VERIFY( m.empty() );

  m.insert_range(Range(a, a+4));
  VERIFY( eq(m, {a, 4}) );

  m.insert_range(Range(a+4, a+7));
  VERIFY( eq(m, {a, 7}) );

  m.insert_range(Range(a, a+9));
  VERIFY( eq(m, {a, 9}) );

  m.insert_range(Range(a, a+14));
  VERIFY( eq(m, {a, 9}) );
}

struct MyPair {
  long x;
  long y;

  constexpr operator std::pair<int const, double>() const
  { return {x, y}; }

  friend bool operator==(MyPair, MyPair) = default;
  constexpr friend bool operator==(MyPair lhs, std::pair<int const, double> rhs)
  { return (lhs.x == rhs.first) && (lhs.y == rhs.second); }
};

template<typename Range>
void
do_test_v()
{
  do_test<Range, int, double>();
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_v<test_forward_range<std::pair<int, double>>>();
  do_test_v<test_forward_range<std::pair<short, float>>>();
  do_test_v<test_forward_range<std::tuple<int, double>>>();
  do_test_v<test_forward_range<MyPair>>();

  return true;
}

int main()
{
  test_ranges();
}
