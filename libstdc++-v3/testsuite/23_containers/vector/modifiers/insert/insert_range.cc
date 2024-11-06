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

  std::vector<V, Alloc> v;
  v.insert_range(v.begin(), Range(a, a));
  VERIFY( v.empty() );
  VERIFY( v.capacity() == 0 );
  v.insert_range(v.begin(), Range(a, a+4));
  VERIFY( eq(v, {a, a+4}) );
  v.clear();
  v.insert_range(v.begin(), Range(a+4, a+9));
  VERIFY( eq(v, {a+4, a+9}) );
  v.insert_range(v.begin(), Range(a, a+4));
  VERIFY( eq(v, a) );
  v.clear();
  v.shrink_to_fit();
  v.insert_range(v.begin(), Range(a, a+3));
  v.insert_range(v.end(), Range(a+6, a+9));
  v.insert_range(v.begin()+3, Range(a+3, a+6));
  VERIFY( eq(v, a) );
  v.resize(3);
  v.insert_range(v.begin()+1, Range(a+4, a+9));
  v.insert_range(v.begin()+1, Range(a+1, a+4));
  v.resize(9);
  VERIFY( eq(v, a) );
  v.insert_range(v.begin() + 6, Range(a, a));
  VERIFY( eq(v, a) );
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

  do_test_a<test_range<int, input_iterator_wrapper_nocopy>>();
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

constexpr bool
test_constexpr()
{
  // XXX: this doesn't test the non-forward_range code paths are constexpr.
  do_test<std::span<short>, std::allocator<int>>();
  return true;
}

int main()
{
  test_ranges();
  static_assert( test_constexpr() );
}
