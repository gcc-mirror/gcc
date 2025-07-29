// { dg-do run { target c++23 } }

#include <vector>
#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

template<typename Range, typename Alloc>
constexpr void
do_test()
{
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,1,0,1,0,0,1,0,0};

  auto eq = [](const std::vector<bool, Alloc>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto i = 0u; i < l.size(); ++i)
      if (l[i] != r[i])
	return false;
    return true;
  };

  // assign to empty vector
  std::vector<bool, Alloc> v;
  v.assign_range(Range(a, a));
  VERIFY( v.empty() );
  VERIFY( v.capacity() == 0 );
  v.assign_range(Range(a, a+4));
  VERIFY( eq(v, {a, 4}) );
  v.clear();
  v.assign_range(Range(a)); // larger than v.capacity()
  VERIFY( eq(v, a) );
  v.clear();
  v.assign_range(Range(a, a+4)); // smaller than v.capacity()
  VERIFY( eq(v, {a, 4}) );
  v.clear();
  v.assign_range(Range(a)); // equal to v.capacity()
  VERIFY( eq(v, a) );

  // assign to non-empty vector
  v.assign_range(Range(a, a+4)); // smaller than size()
  VERIFY( eq(v, {a, 4}) );
  v.assign_range(Range(a)); // larger than size(), equal to capacity()
  VERIFY( eq(v, a) );
  v.resize(1);
  v.assign_range(Range(a, a+4)); // larger than size(), smaller than capacity()
  VERIFY( eq(v, {a, 4}) );
  v.clear();
  v.resize(4);
  v.assign_range(Range(a, a+4)); // equal to size(), smaller than capacity()
  VERIFY( eq(v, {a, 4}) );
  v.shrink_to_fit();
  v.assign_range(Range(a)); // larger than capacity()
  VERIFY( eq(v, a) );
  v.assign_range(Range(a, a));
  VERIFY( v.empty() );
}

template<typename Range>
constexpr void
do_test_a()
{
  do_test<Range, std::allocator<bool>>();
  do_test<Range, __gnu_test::SimpleAllocator<bool>>();
}

constexpr bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_a<test_forward_range<bool>>();
  do_test_a<test_forward_sized_range<bool>>();
  do_test_a<test_sized_range_sized_sent<bool, forward_iterator_wrapper>>();

  do_test_a<test_input_range<bool>>();
  do_test_a<test_input_sized_range<bool>>();
  do_test_a<test_sized_range_sized_sent<bool, input_iterator_wrapper>>();

  do_test_a<test_range<bool, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range<bool, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range_sized_sent<bool, input_iterator_wrapper_nocopy>>();

  do_test_a<test_forward_range<short>>();
  do_test_a<test_input_range<short>>();

  // Not lvalue-convertible to bool
  struct C {
    constexpr C(bool v) : val(v) { }
    constexpr operator bool() && { return val; }
    constexpr bool operator==(bool b) const { return b == val; }
    bool val;
  };
  using rvalue_input_range = test_range<C, input_iterator_wrapper_rval>;
  do_test_a<rvalue_input_range>();

  return true;
}

int main()
{
  test_ranges();
  static_assert( test_ranges() );
}
