// { dg-do run { target c++23 } }

#include <deque>
#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

template<typename Range, typename Alloc>
constexpr void
do_test()
{
  // The deque's value_type.
  using V = typename std::allocator_traits<Alloc>::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [](const std::deque<V, Alloc>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto i = 0u; i < l.size(); ++i)
      if (l[i] != r[i])
	return false;
    return true;
  };

  // assign to empty deque
  std::deque<V, Alloc> d;
  d.assign_range(Range(a, a));
  VERIFY( d.empty() );
  d.assign_range(Range(a, a+4));
  VERIFY( eq(d, {a, 4}) );
  d.clear();
  d.assign_range(Range(a, a+9));
  VERIFY( eq(d, a) );
  d.clear();
  d.assign_range(Range(a, a+4));
  VERIFY( eq(d, {a, 4}) );
  d.clear();
  d.assign_range(Range(a, a+9));
  VERIFY( eq(d, a) );


  // assign to non-empty deque
  d.assign_range(Range(a, a+4)); // smaller than size()
  VERIFY( eq(d, {a, 4}) );
  d.assign_range(Range(a, a+9)); // larger than size()
  VERIFY( eq(d, a) );
  d.resize(1);
  d.assign_range(Range(a, a+4)); // larger than size()
  VERIFY( eq(d, {a, 4}) );
  d.clear();
  d.resize(4);
  d.assign_range(Range(a, a+4)); // equal to size()
  VERIFY( eq(d, {a, 4}) );
  d.shrink_to_fit();
  d.assign_range(Range(a, a+9));
  VERIFY( eq(d, a) );
  d.assign_range(Range(a, a));
  VERIFY( d.empty() );
}

template<typename Range>
void
do_test_a()
{
  do_test<Range, std::allocator<int>>();
  do_test<Range, __gnu_test::SimpleAllocator<int>>();
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_a<test_forward_range<int>>();
  do_test_a<test_forward_sized_range<int>>();
  do_test_a<test_sized_range_sized_sent<int, forward_iterator_wrapper>>();

  do_test_a<test_input_range<int>>();
  do_test_a<test_input_sized_range<int>>();
  do_test_a<test_sized_range_sized_sent<int, input_iterator_wrapper>>();

  do_test_a<test_range_nocopy<int, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range<int, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range_sized_sent<int, input_iterator_wrapper_nocopy>>();

  do_test_a<test_forward_range<short>>();
  do_test_a<test_input_range<short>>();

  // Not lvalue-convertible to int
  struct C {
    C(int v) : val(v) { }
    operator int() && { return val; }
    bool operator==(int b) const { return b == val; }
    int val;
  };
  using rvalue_input_range = test_range<C, input_iterator_wrapper_rval>;
  do_test<rvalue_input_range, std::allocator<int>>();

  return true;
}

int main()
{
  test_ranges();
}
