// { dg-do run { target c++23 } }

#include <set>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <set>"
#endif

#include <algorithm>
#include <ranges>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

struct StateCmp {
  int state = 7;

  template<typename T, typename U>
  bool operator()(T const& l, U const & r) const {
    return l > r;
  }
};

void
test_deduction_guide(long* p)
{
  __gnu_test::test_input_range<long> r(p, p);
  std::set s(std::from_range, r);
  static_assert(std::is_same_v<decltype(s), std::set<long>>);

  StateCmp cmp;
  std::set s2(std::from_range, r, cmp);
  static_assert(std::is_same_v<decltype(s2), std::set<long, StateCmp>>);

  using Alloc = __gnu_test::SimpleAllocator<long>;
  Alloc alloc;
  std::set s3(std::from_range, r, alloc);
  static_assert(std::is_same_v<decltype(s3), std::set<long, std::less<long>, Alloc>>);

  std::set s4(std::from_range, r, cmp, alloc);
  static_assert(std::is_same_v<decltype(s4), std::set<long, StateCmp, Alloc>>);
}

template<typename T, typename U>
constexpr bool is_equal(std::less<T>, std::less<U>)
{ return true; }

constexpr bool is_equal(StateCmp lhs, StateCmp rhs)
{ return lhs.state = rhs.state; }

template<typename Range, typename Alloc, typename Cmp>
constexpr void
do_test(Alloc alloc, Cmp cmp)
{
  // The set's value_typ.
  using V = typename Alloc::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,1,2,3,4,5};

  auto eq = [&](std::set<V, Cmp, Alloc> const& l, std::span<T> r) {
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

  std::set<V, Cmp, Alloc> s0(std::from_range, Range(a, a+0));
  VERIFY( s0.empty() );
  VERIFY( s0.get_allocator() == Alloc() );
  VERIFY( is_equal(s0.key_comp(), Cmp()) );

  std::set<V, Cmp, Alloc> s4(std::from_range, Range(a, a+4), cmp);
  VERIFY( eq(s4, {a, 4}) );
  VERIFY( s4.get_allocator() == Alloc() );
  VERIFY( is_equal(s4.key_comp(), Cmp()) );

  std::set<V, Cmp, Alloc> s9(std::from_range, Range(a, a+9), alloc);
  VERIFY( eq(s9, {a, 9}) );
  VERIFY( s9.get_allocator() == alloc );
  VERIFY( is_equal(s9.key_comp(), cmp) );

  std::set<V, Cmp, Alloc> sr(std::from_range, Range(a, a+14), cmp, alloc);
  VERIFY( eq(sr, {a, 9}) );
  VERIFY( sr.get_allocator() == alloc );
  VERIFY( is_equal(sr.key_comp(), cmp) );
}

template<typename Range>
void
do_test_ac()
{
  do_test<Range>(std::allocator<int>(), std::less<int>());
  do_test<Range>(std::allocator<int>(), StateCmp{17});
  do_test<Range>(__gnu_test::uneq_allocator<int>(42), std::less<int>());
  do_test<Range>(__gnu_test::uneq_allocator<int>(42), StateCmp{17});
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_ac<test_forward_range<int>>();
  do_test_ac<test_range_nocopy<int, input_iterator_wrapper_nocopy>>();
  do_test_ac<test_forward_range<short>>();

  return true;
}

int main()
{
  test_ranges();
}
