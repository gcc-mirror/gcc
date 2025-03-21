// { dg-do run { target c++23 } }

#include <queue>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <queue>"
#endif

#include <list>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

void
test_deduction_guide(long* p)
{
  __gnu_test::test_input_range<long> r(p, p);
  std::queue q(std::from_range, r);
  static_assert(std::is_same_v<decltype(q), std::queue<long>>);

  using Alloc = __gnu_test::SimpleAllocator<long>;
  Alloc alloc;
  std::queue q2(std::from_range, r, alloc);
  static_assert(std::is_same_v<decltype(q2), std::queue<long, std::deque<long, Alloc>>>);
}

template<typename Range, typename Cont>
constexpr void
do_test()
{
  // The queue's value_type.
  using V = typename Cont::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [](std::queue<V, Cont>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto const& v : r) {
      if (v != l.front())
        return false;
      l.pop();
    }
    return true;
  };

  std::queue<V, Cont> q0(std::from_range, Range(a, a+0));
  VERIFY( q0.empty() );

  std::queue<V, Cont> q4(std::from_range, Range(a, a+4));
  VERIFY( eq(q4, {a, 4}) );

  typename Cont::allocator_type alloc;
  std::queue<V, Cont> q9(std::from_range, Range(a, a+9), alloc);
  VERIFY( eq(q9, {a, 9}) );
}

template<typename T, typename Alloc = std::allocator<T>>
struct NoFromRangeCont : std::deque<T, Alloc>
{
  NoFromRangeCont() = default;
  NoFromRangeCont(const Alloc& a) : std::deque<T, Alloc>(a) {}
};

template<typename Range>
void
do_test_c()
{
  do_test<Range, std::deque<int>>();
  do_test<Range, std::deque<int, __gnu_test::SimpleAllocator<int>>>();
  do_test<Range, std::list<int>>();
  do_test<Range, NoFromRangeCont<int>>();
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
