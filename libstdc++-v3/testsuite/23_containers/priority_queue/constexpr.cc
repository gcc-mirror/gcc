// { dg-do compile { target c++26 } }

#include <queue>

#ifndef __cpp_lib_constexpr_queue
# error "Feature test macro for __cpp_lib_constexpr_queue is missing in <queue>"
#elif __cpp_lib_constexpr_queue != 202502L
# error "Feature test macro for __cpp_lib_constexpr_queue has wrong value in <queue>"
#endif

#include <algorithm>
#include <ranges>
#include <functional>
#include <vector>
#include <numeric>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

using namespace __gnu_test;

template<typename Cont, typename Cmp = std::less<int>>
constexpr bool
ctor_tests(Cmp cmp = Cmp())
{
  using V = typename Cont::value_type;
  using Alloc = Cont::allocator_type;

  V rg[] {2, 3, 5, 7};
  Cont v0{std::from_range, rg};
  Cont v1 = v0;
  Cont v2 = v0;
  Alloc alloc;
  auto top_range = * std::ranges::max_element(v0, cmp);

  auto eq = [&] (std::priority_queue<V, Cont, Cmp>& l, std::span<V> r) {
    if (l.size() != r.size())
      return false;

    std::vector<V> s(r.begin(), r.end());
    std::ranges::sort(s, cmp);
    for (auto const& v : s | std::views::reverse) {
      if (v != l.top())
	return false;
      l.pop();
    }
    return true;
  };

  std::priority_queue<V, Cont, Cmp> pq1;
  VERIFY( pq1.size() == 0 && pq1.empty() );

  std::priority_queue<V, Cont, Cmp> pq2(cmp);
  VERIFY( pq2.size() == 0 && pq2.empty() );

  std::priority_queue<V, Cont, Cmp> pq3(cmp, v0);
  VERIFY( pq3.size() == 4 && pq3.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq4(cmp, std::move(v0));
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq5(pq3);
  VERIFY( pq5.top() == pq3.top() );
  VERIFY( pq5.size() == pq3.size() );

  std::priority_queue<V, Cont, Cmp> pq6(std::move(pq3));
  VERIFY( pq6.top() == pq5.top() );
  VERIFY( pq6.size() == pq5.size() );

  std::priority_queue<V, Cont, Cmp>
    pq10(std::begin(rg), std::end(rg),  cmp);
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp>
    pq11(std::begin(rg), std::end(rg), cmp, v0);
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp>
    pq12(std::begin(rg), std::end(rg), cmp, std::move(v0));
  VERIFY( pq4.size() == 4 && pq4.top() == top_range );

  std::priority_queue<V, Cont, Cmp> pq14(alloc);
  VERIFY( pq14.empty() );

  std::priority_queue<V, Cont, Cmp> pq15(cmp, alloc);
  VERIFY( pq15.empty() );

  std::priority_queue<V, Cont, Cmp> pq16(cmp, v2, alloc);
  VERIFY( eq(pq16, {rg, 4}) );

  std::priority_queue<V, Cont, Cmp> pq17(cmp, std::move(v2), alloc);
  VERIFY( eq(pq17, {rg, 4}) );

  std::priority_queue<V, Cont, Cmp> pq18(pq12, alloc);
  VERIFY( pq18.size() == pq12.size());

  std::priority_queue<V, Cont, Cmp> pq19(std::move(pq12), alloc);
  VERIFY( pq19.size() == pq18.size() );
  return true;
}

static_assert( ctor_tests<std::vector<int>>() );
static_assert( ctor_tests<std::vector<int>, std::greater<int>>() );
static_assert( ctor_tests<std::vector<int, SimpleAllocator<int>>>() );
static_assert( ctor_tests<std::deque<int>>() );
static_assert( ctor_tests<std::deque<int>, std::greater<int>>() );
static_assert( ctor_tests<std::deque<int, SimpleAllocator<int>>>() );

constexpr bool
push_and_pop_test()
{
  std::priority_queue<int> a;
  a.push(2);
  a.push(4);
  VERIFY( a.top() == 4 );
  a.pop();
  VERIFY( a.top() == 2 );
  a.pop();
  VERIFY( a.empty() );
  return true;
}
static_assert( push_and_pop_test() );

constexpr int swap_test()
{
  std::priority_queue<int> a,b;
  a.push(2);
  b.push(4);
  std::swap(a, b);
  VERIFY( a.top() == 4 );
  VERIFY( b.top() == 2 );
  return true;
}
static_assert (swap_test());

constexpr bool
emplace_test()
{
  struct S
  {
    int foo;
    constexpr S(int i, int j) : foo{i + j} {}
    std::strong_ordering operator<=>(const S&) const = default;
  };

  std::priority_queue<S> pq;
  pq.emplace(0, 0);
  pq.emplace(1, 0);
  VERIFY( pq.size() == 2 );
  VERIFY( pq.top().foo == 1 );
  pq.pop();
  VERIFY ( pq.top().foo == 0 );
  return true;
}
static_assert( emplace_test() );

template<typename Range, typename Cont, typename Cmp>
constexpr void
do_range_tests(Cmp cmp = Cmp())
{
  using V = typename Cont::value_type;
  using Alloc = typename Cont::allocator_type;
  using T = std::ranges::range_value_t<Range>;

  Alloc alloc;
  T rg[] {2, 3, 5, 7, 11, 13};

  auto top_range = std::ranges::max_element(rg, cmp);

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

  std::priority_queue<V, Cont, Cmp>
    pq1(std::from_range, Range(rg, rg+4), cmp);
  VERIFY( eq(pq1, {rg, 4}) );

  std::priority_queue<V, Cont, Cmp>
    pq2(std::from_range, Range(rg, rg+5), alloc);
  VERIFY( eq(pq2, {rg, 5}) );

  std::priority_queue<V, Cont, Cmp>
    pq3(std::from_range, Range(rg, rg+6), cmp, alloc);
  VERIFY( eq(pq3, {rg, 6}) );

  std::priority_queue<V, Cont, Cmp> pq4;
  pq4.push_range(Range(rg, rg+6));
  VERIFY( eq(pq4, {rg, 6}) );
}

template<typename Range, typename Cont>
constexpr void
do_ranges_tests_b()
{
  do_range_tests<Range, Cont, std::less<>>();
  do_range_tests<Range, Cont, std::greater<>>();
}

template<typename Cont>
constexpr bool
ranges_tests() {
  using T = typename Cont::value_type;
  do_ranges_tests_b<
    test_forward_range<T>, Cont>();
  do_ranges_tests_b<
    test_forward_sized_range<T>, Cont>();
  do_ranges_tests_b<
    test_sized_range_sized_sent<T, forward_iterator_wrapper>, Cont>();

  do_ranges_tests_b<
    test_input_range<T>, Cont>();
  do_ranges_tests_b<
    test_input_sized_range<T>, Cont>();
  do_ranges_tests_b<
    test_sized_range_sized_sent<T, forward_iterator_wrapper>, Cont>();

  do_ranges_tests_b<
    test_range<T, input_iterator_wrapper_nocopy>, Cont>();
  do_ranges_tests_b<
    test_sized_range<T, input_iterator_wrapper_nocopy>, Cont>();
  do_ranges_tests_b<
    test_sized_range_sized_sent<T, input_iterator_wrapper_nocopy>, Cont>();
  return true;
}

static_assert( ranges_tests<std::vector<int>>() );
static_assert( ranges_tests<std::vector<int, SimpleAllocator<int>>>() );
static_assert( ranges_tests<std::deque<int, SimpleAllocator<int>>>() );
