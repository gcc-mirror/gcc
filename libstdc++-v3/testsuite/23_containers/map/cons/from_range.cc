// { dg-do run { target c++23 } }

#include <map>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <map>"
#endif

#include <algorithm>
#include <ranges>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <tuple>

struct StateCmp {
  int state = 7;

  template<typename T, typename U>
  bool operator()(T const& l, U const & r) const
  { return l > r; }
};

void
test_deduction_guide()
{
  __gnu_test::test_input_range<std::pair<long, float>> r(0, 0);
  std::map m(std::from_range, r);
  static_assert(std::is_same_v<decltype(m), std::map<long, float>>);

  StateCmp cmp;
  std::map m2(std::from_range, r, cmp);
  static_assert(std::is_same_v<decltype(m2), std::map<long, float, StateCmp>>);

  using Alloc = __gnu_test::SimpleAllocator<std::pair<const long, float>>;
  Alloc alloc;
  std::map m3(std::from_range, r, alloc);
  static_assert(std::is_same_v<decltype(m3), std::map<long, float, std::less<long>, Alloc>>);

  std::map m4(std::from_range, r, cmp, alloc);
  static_assert(std::is_same_v<decltype(m4), std::map<long, float, StateCmp, Alloc>>);

  __gnu_test::test_input_range<std::pair<const long, const float>> r2(0, 0);
  std::map m5(std::from_range, r2);
  static_assert(std::is_same_v<decltype(m5), std::map<long, const float>>);

  // LWG4223: deduces map<const long&, float&>
  //__gnu_test::test_input_range<std::pair<const long&, float&>> r3(0, 0);
  // std::map m6(std::from_range, r3);

  __gnu_test::test_input_range<std::tuple<long, float>> r4(0, 0);
  std::map m7(std::from_range, r4);
  static_assert(std::is_same_v<decltype(m7), std::map<long, float>>);
  std::map it7(r4.begin(), r4.begin());
  static_assert(std::is_same_v<decltype(it7), std::map<long, float>>);
}

template<typename T, typename U>
constexpr bool is_equal(std::less<T>, std::less<U>)
{ return true; }

constexpr bool is_equal(StateCmp lhs, StateCmp rhs)
{ return lhs.state = rhs.state; }

constexpr auto get0 = [](auto const& t) {
  using std::get;
  return get<0>(t);
};

template<typename Range, typename Alloc, typename Cmp>
constexpr void
do_test(Alloc alloc, Cmp cmp)
{
  // The map's value_type, key_type and mapped_type.
  using P = typename Alloc::value_type;
  using K = typename P::first_type;
  using V = typename P::second_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{{1,2},{2,3},{3,4},{4,5},{5,6},{6,7},{7,8},{8,9},{9,0},
	{1,1},{2,2},{3,3},{4,4},{5,5}};

  auto eq = [&](std::map<K, V, Cmp, Alloc> const& l, std::span<T> r) {
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

  std::map<K, V, Cmp, Alloc> m0(std::from_range, Range(a, a+0));
  VERIFY( m0.empty() );
  VERIFY( m0.get_allocator() == Alloc() );
  VERIFY( is_equal(m0.key_comp(), Cmp()) );

  std::map<K, V, Cmp, Alloc> m4(std::from_range, Range(a, a+4), cmp);
  VERIFY( eq(m4, {a, 4}) );
  VERIFY( m4.get_allocator() == Alloc() );
  VERIFY( is_equal(m4.key_comp(), Cmp()) );

  std::map<K, V, Cmp, Alloc> m9(std::from_range, Range(a, a+9), alloc);
  VERIFY( eq(m9, {a, 9}) );
  VERIFY( m9.get_allocator() == alloc );
  VERIFY( is_equal(m9.key_comp(), cmp) );

  std::map<K, V, Cmp, Alloc> mr(std::from_range, Range(a, a+14), cmp, alloc);
  VERIFY( eq(mr, {a, 9}) );
  VERIFY( mr.get_allocator() == alloc );
  VERIFY( is_equal(mr.key_comp(), cmp) );
}

template<typename Range>
void
do_test_ac()
{
  do_test<Range>(std::allocator<std::pair<const int, double>>(), std::less<int>());
  do_test<Range>(std::allocator<std::pair<const int, double>>(), StateCmp{17});
  do_test<Range>(__gnu_test::uneq_allocator<std::pair<const int, double>>(42), std::less<int>());
  do_test<Range>(__gnu_test::uneq_allocator<std::pair<const int, double>>(42), StateCmp{17});
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

  do_test_ac<test_forward_range<std::pair<int, double>>>();
  do_test_ac<test_range_nocopy<std::pair<int, double>, input_iterator_wrapper_nocopy>>();
  do_test_ac<test_forward_range<std::pair<short, float>>>();
  do_test_ac<test_forward_range<std::tuple<int, double>>>();
  do_test_ac<test_forward_range<MyPair>>();

  return true;
}

int main()
{
  test_ranges();
}
