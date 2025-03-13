// { dg-do run { target c++23 } }

#include <algorithm>
#include <ranges>
#include <set>
#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

struct Gt {
  template<typename T, typename U>
  bool operator()(T const& l, U const & r) const {
    return l > r;
  }
};

template<typename Range, typename V, typename Cmp>
constexpr void
do_test(Cmp cmp = Cmp())
{
  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [&](std::set<V, Cmp> const& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;

    std::vector<T> s(r.begin(), r.end());
    std::ranges::sort(s, cmp);
    for (auto const& [vl, vr] : std::views::zip(l, s)) {
      if (vl != vr)
	return false;
    }
    return true;
  };

  std::set<V, Cmp> s;
  s.insert_range(Range(a, a+0));
  VERIFY( s.empty() );

  s.insert_range(Range(a, a+4));
  VERIFY( eq(s, {a, 4}) );

  s.insert_range(Range(a+4, a+7));
  VERIFY( eq(s, {a, 7}) );

  s.insert_range(Range(a, a+9));
  VERIFY( eq(s, {a, 9}) );

  s.insert_range(Range(a, a+9));
  VERIFY( eq(s, {a, 9}) );
}

template<typename Range>
void
do_test_c()
{
  do_test<Range, int, std::less<int>>();
  do_test<Range, int, Gt>();
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_c<test_forward_range<int>>();
  do_test_c<test_range_nocopy<int, input_iterator_wrapper_nocopy>>();
  do_test_c<test_forward_range<short>>();

  return true;
}

int main()
{
  test_ranges();
}
