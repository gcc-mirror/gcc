// { dg-do run { target c++23 } }

#include <algorithm>
#include <map>
#include <ranges>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <tuple>

struct Gt {
  template<typename T, typename U>
  bool operator()(T const& l, U const & r) const
  { return l > r; }
};

constexpr auto get0 = [](auto const& t) {
  using std::get;
  return get<0>(t);
};

template<typename Range, typename K, typename V, typename Cmp>
constexpr void
do_test(Cmp cmp = Cmp())
{
  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{{1,2},{2,3},{3,4},{4,5},{5,6},{6,7},{7,8},{8,9},{9,0},
	{1,1},{2,2},{3,3},{4,4},{5,5}};

  auto eq = [&](std::map<K, V, Cmp> const& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;

    std::vector<T> s(r.begin(), r.end());
    std::ranges::sort(s, cmp, get0);
    for (auto const& [vl, vr] : std::views::zip(l, s)) {
      if (vl != vr)
	return false;
    }
    return true;
  };

  std::map<K, V, Cmp> s;
  VERIFY( s.empty() );

  s.insert_range(Range(a, a+4));
  VERIFY( eq(s, {a, 4}) );

  s.insert_range(Range(a+4, a+9));
  VERIFY( eq(s, {a, 9}) );

  s.insert_range(Range(a, a+14));
  VERIFY( eq(s, {a, 9}) );

  s.insert_range(Range(a, a+14));
  VERIFY( eq(s, {a, 9}) );
}

template<typename Range>
void
do_test_c()
{
  do_test<Range, int, double, std::less<int>>();
  do_test<Range, int, double, Gt>();
}

struct MyPair {
  long x;
  long y;

  constexpr operator std::pair<int const, double>() const
  { return {x, y}; }

  template<unsigned I>
    requires (I < 2)
  friend constexpr long get(MyPair p)
  { return (I == 0) ? p.x : p.y; }

  constexpr friend bool operator==(MyPair lhs, std::pair<int const, double> rhs)
  { return (lhs.x == rhs.first) && (lhs.y == rhs.second); }
};

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_c<test_forward_range<std::pair<int, double>>>();
  do_test_c<test_range_nocopy<std::pair<int, double>, input_iterator_wrapper_nocopy>>();
  do_test_c<test_forward_range<std::pair<short, float>>>();
  do_test_c<test_forward_range<std::tuple<int, double>>>();
  do_test_c<test_forward_range<MyPair>>();

  return true;
}

int main()
{
  test_ranges();
}
