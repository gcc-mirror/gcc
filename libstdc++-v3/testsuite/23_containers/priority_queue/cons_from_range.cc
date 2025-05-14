// { dg-do run { target c++23 } }

#include <queue>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <queue>"
#endif

#include <algorithm>
#include <ranges>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

struct Gt {
  template<typename T, typename U>
  bool operator()(T const& l, U const & r) {
    return l > r;
  }
};

void
test_deduction_guide(long* p)
{
  __gnu_test::test_input_range<long> r(p, p);
  std::priority_queue pq(std::from_range, r);
  static_assert(std::is_same_v<decltype(pq), std::priority_queue<long>>);

  Gt cmp;
  std::priority_queue pq3(std::from_range, r, cmp);
  static_assert(std::is_same_v<decltype(pq3), std::priority_queue<long, std::vector<long>, Gt>>);

  using Alloc = __gnu_test::SimpleAllocator<long>;
  Alloc alloc;
  std::priority_queue pq2(std::from_range, r, alloc);
  static_assert(std::is_same_v<decltype(pq2), std::priority_queue<long, std::vector<long, Alloc>>>);

  std::priority_queue pq4(std::from_range, r, cmp, alloc);
  static_assert(std::is_same_v<decltype(pq4), std::priority_queue<long, std::vector<long, Alloc>, Gt>>);
}

template<typename Range, typename Cont, typename Cmp = std::less<int>>
constexpr void
do_test(Cmp cmp = Cmp())
{
  // The queue's value_type.
  using V = typename Cont::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [&](std::priority_queue<V, Cont, Cmp>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;

    std::vector<T> s(r.begin(), r.end());
    std::ranges::sort(s, cmp);
    for (auto const& v : s | std::views::reverse) {
      if (v != l.top())
        return false;
      l.pop();
    }
    return true;
  };

  std::priority_queue<V, Cont, Cmp> pq0(std::from_range, Range(a, a+0));
  VERIFY( pq0.empty() );

  std::priority_queue<V, Cont, Cmp> pq4(std::from_range, Range(a, a+4), cmp);
  VERIFY( eq(pq4, {a, 4}) );

  typename Cont::allocator_type alloc;
  std::priority_queue<V, Cont, Cmp> pq7(std::from_range, Range(a, a+7), alloc);
  VERIFY( eq(pq7, {a, 7}) );

  std::priority_queue<V, Cont, Cmp> pq9(std::from_range, Range(a, a+9), cmp, alloc);
  VERIFY( eq(pq9, {a, 9}) );
}

template<typename T, typename Alloc = std::allocator<T>>
struct NoFromRangeCont : std::vector<T, Alloc>
{
  NoFromRangeCont() = default;
  NoFromRangeCont(const Alloc& a) : std::vector<T, Alloc>(a) {}
};

template<typename Range>
void
do_test_c()
{
  do_test<Range, std::vector<int>>();
  do_test<Range, std::vector<int, __gnu_test::SimpleAllocator<int>>>();
  do_test<Range, std::vector<int>, Gt>();
  do_test<Range, std::vector<int, __gnu_test::SimpleAllocator<int>>, Gt>();
  do_test<Range, std::deque<int>>();
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
