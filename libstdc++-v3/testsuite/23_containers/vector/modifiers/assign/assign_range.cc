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
  // The vector's value_type.
  using V = typename std::allocator_traits<Alloc>::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [](const std::vector<V, Alloc>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto i = 0u; i < l.size(); ++i)
      if (l[i] != r[i])
	return false;
    return true;
  };

  // assign to empty vector
  std::vector<V, Alloc> v;
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
  do_test<Range, std::allocator<int>>();
  do_test<Range, __gnu_test::SimpleAllocator<int>>();
}

constexpr bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_a<test_forward_range<int>>();
  do_test_a<test_forward_sized_range<int>>();
  do_test_a<test_sized_range_sized_sent<int, forward_iterator_wrapper>>();

  do_test_a<test_input_range<int>>();
  do_test_a<test_input_sized_range<int>>();
  do_test_a<test_sized_range_sized_sent<int, input_iterator_wrapper>>();

  do_test_a<test_range<int, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range<int, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range_sized_sent<int, input_iterator_wrapper_nocopy>>();

  do_test_a<test_forward_range<short>>();
  do_test_a<test_input_range<short>>();

  // Not lvalue-convertible to int
  struct C {
    constexpr C(int v) : val(v) { }
    constexpr operator int() && { return val; }
    constexpr bool operator==(int b) const { return b == val; }
    int val;
  };
  using rvalue_input_range = test_range<C, input_iterator_wrapper_rval>;
  do_test<rvalue_input_range, std::allocator<int>>();

  return true;
}

int main()
{
  test_ranges();
  static_assert( test_ranges() );
}
