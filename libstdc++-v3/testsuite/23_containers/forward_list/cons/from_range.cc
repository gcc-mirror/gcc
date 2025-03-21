// { dg-do run { target c++23 } }

#include <forward_list>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <forward_list>"
#endif

#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>
void
test_deduction_guide(long* p)
{
  __gnu_test::test_input_range<long> r(p, p);
  std::forward_list v(std::from_range, r);
  static_assert(std::is_same_v<decltype(v), std::forward_list<long>>);

  using Alloc = __gnu_test::SimpleAllocator<long>;
  Alloc alloc;
  std::forward_list v2(std::from_range, r, alloc);
  static_assert(std::is_same_v<decltype(v2), std::forward_list<long, Alloc>>);
}

template<typename Range, typename Alloc>
constexpr void
do_test(Alloc alloc)
{
  // The forward_list's value_type.
  using V = typename std::allocator_traits<Alloc>::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [](const std::forward_list<V, Alloc>& l, std::span<T> r) {
    if (std::distance(l.begin(), l.end()) != r.size())
      return false;
    unsigned i = 0;
    for (auto& e : l)
      if (e != r[i++])
	return false;
    return true;
  };

  std::forward_list<V, Alloc> v0(std::from_range, Range(a, a+0));
  VERIFY( v0.empty() );
  VERIFY( v0.get_allocator() == Alloc() );

  std::forward_list<V, Alloc> v4(std::from_range, Range(a, a+4));
  VERIFY( eq(v4, {a, 4}) );
  VERIFY( v4.get_allocator() == Alloc() );

  std::forward_list<V, Alloc> v9(std::from_range, Range(a, a+9), alloc);
  VERIFY( eq(v9, {a, 9}) );
  VERIFY( v9.get_allocator() == alloc );
}

template<typename Range>
void
do_test_a()
{
  do_test<Range>(std::allocator<int>());
  do_test<Range>(__gnu_test::uneq_allocator<int>(42));
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

  // Not lvalue-convertible to bool
  struct C {
    C(int v) : val(v) { }
    operator int() && { return val; }
    bool operator==(int b) const { return b == val; }
    int val;
  };
  using rvalue_input_range = test_range<C, input_iterator_wrapper_rval>;
  do_test<rvalue_input_range>(std::allocator<int>());

  return true;
}

int main()
{
  test_ranges();
}
