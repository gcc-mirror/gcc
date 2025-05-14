// { dg-do run { target c++23 } }

#include <deque>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <deque>"
#endif

#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

void
test_deduction_guide(long* p)
{
  __gnu_test::test_input_range<long> r(p, p);
  std::deque d(std::from_range, r);
  static_assert(std::is_same_v<decltype(d), std::deque<long>>);

  using Alloc = __gnu_test::SimpleAllocator<long>;
  Alloc alloc;
  std::deque d2(std::from_range, r, alloc);
  static_assert(std::is_same_v<decltype(d2), std::deque<long, Alloc>>);
}

template<typename Range, typename Alloc>
constexpr void
do_test(Alloc alloc)
{
  // The deque's value_type.
  using V = typename std::allocator_traits<Alloc>::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14};

  auto eq = [](const std::deque<V, Alloc>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto i = 0u; i < l.size(); ++i)
      if (l[i] != r[i])
	return false;
    return true;
  };

  std::deque<V, Alloc> d0(std::from_range, Range(a, a+0));
  VERIFY( d0.empty() );
  VERIFY( d0.get_allocator() == Alloc() );

  std::deque<V, Alloc> d4(std::from_range, Range(a, a+4));
  VERIFY( eq(d4, {a, 4}) );
  VERIFY( d4.get_allocator() == Alloc() );

  std::deque<V, Alloc> d8(std::from_range, Range(a, a+8));
  VERIFY( eq(d8, {a, 8}) );
  VERIFY( d8.get_allocator() == Alloc() );

  std::deque<V, Alloc> d9(std::from_range, Range(a, a+14), alloc);
  VERIFY( eq(d9, {a, 14}) );
  VERIFY( d9.get_allocator() == alloc );
}

struct EightInBuf
{
  EightInBuf(int x) : elems{x}
  { }

private:
  int elems[512 / (sizeof(int) * 8)];

  friend constexpr bool operator==(EightInBuf const& lhs, int rhs)
  { return lhs.elems[0] == rhs; }
};


template<typename Range>
void
do_test_a()
{
  do_test<Range>(std::allocator<int>());
  do_test<Range>(__gnu_test::uneq_allocator<int>(42));
  do_test<Range>(std::allocator<EightInBuf>());
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
  do_test<rvalue_input_range>(std::allocator<int>());

  return true;
}

int main()
{
  test_ranges();
}
