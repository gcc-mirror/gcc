// { dg-do compile { target c++26 } }

#include <queue>

#ifndef __cpp_lib_constexpr_queue
# error "Feature test macro for __cpp_lib_constexpr_queue is missing in <queue>"
#elif __cpp_lib_constexpr_queue != 202502L
# error "Feature test macro for __cpp_lib_constexpr_queue has wrong value in <queue>"
#endif

#include <ranges>
#include <functional>
#include <vector>
#include <numeric>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

using namespace __gnu_test;

template<typename Cont>
constexpr bool
ctor_tests()
{
  using Tp = typename Cont::value_type;
  using Alloc = typename Cont::allocator_type;

  auto eq = [] (std::queue<Tp, Cont> l, std::span<Tp> r) {
    if (l.size() != r.size())
      return false;

    std::vector<Tp> s(r.begin(), r.end());
    for (auto v : s) {
      if (v != l.front())
	return false;
      l.pop();
    }
    return true;
  };

  Cont c0;
  Alloc alloc0;

  std::queue<Tp, Cont> q1(c0);
  VERIFY( q1.size() == 0 && q1.empty() );
  q1.push(1);
  q1.push(2);
  VERIFY( q1.size() == 2) ;

  Cont c1{1, 2};
  std::queue<Tp, Cont> q2(c1);
  VERIFY ( q2 == q1 );
  std::queue<Tp, Cont> q3(std::move(c1));
  VERIFY ( q3 == q1 );

  std::queue<Tp, Cont> q4(q1);
  std::queue<Tp, Cont> q5(std::move(q1));
  VERIFY ( q4 == q5 );

  Tp rg[4] = {2, 3, 5, 7};
  std::queue<Tp, Cont> q6(std::begin(rg), std::end(rg));
  VERIFY ( eq(q6, rg) );

  VERIFY( q6.size() == std::size(rg));
  VERIFY( q6.front() == 2 );
  q6.pop();
  VERIFY( q6.front() == 3 );
  q6.pop();
  VERIFY( q6.front() == 5 );
  q6.pop();
  VERIFY( q6.front() == 7 );
  q6.pop();

  std::queue<Tp, Cont> q7(alloc0);
  q7.push(1);
  q7.push(2);
  VERIFY( q7.size() == 2 );

  Cont c2{1, 2};
  std::queue<Tp, Cont> q8(c2, alloc0);
  VERIFY( q8 == q7 );
  std::queue<Tp, Cont> q9(std::move(c2), alloc0);
  VERIFY( q9 == q7 );
  VERIFY( c2.empty() );

  std::queue<Tp, Cont> q10(q7, alloc0);
  VERIFY( q10 == q7 );
  VERIFY( q10.size() == q7.size() );
  VERIFY( q10.front() == q7.front() );
  VERIFY( q10.back() == q7.back() );

  std::queue<Tp, Cont> q11(std::move(q7), alloc0);
  VERIFY( q11 == q10 );
  VERIFY( q11.size() == q10.size() );
  VERIFY( q7.empty() );

  std::queue<Tp, Cont> q12(std::begin(rg), std::end(rg), alloc0);
  VERIFY ( eq(q12, rg) );
  VERIFY( q12.size() == std::size(rg));
  VERIFY( q12.front() == 2 );
  q12.pop();
  VERIFY( q12.front() == 3 );
  q12.pop();
  VERIFY( q12.front() == 5 );
  q12.pop();
  VERIFY( q12.front() == 7 );
  q12.pop();

  std::queue<Tp, Cont> q13(std::from_range, rg);
  VERIFY( eq(q13, rg) );
  std::queue<Tp, Cont> q14(std::from_range, rg, alloc0);
  VERIFY( eq(q14, rg) );

  return true;
}
// TODO check list when avaialble
static_assert( ctor_tests<std::deque<int>>() );
static_assert( ctor_tests<std::deque<int, SimpleAllocator<int>>>() );

template<typename Range, typename Cont>
constexpr void
do_ranges_tests_a()
{
  using Tp = typename Cont::value_type;
  typename Cont::allocator_type alloc;
  Tp a[] {2, 3, 5, 7};

  auto eq = [&] (auto l, auto r) {
    if (l.size() != r.size())
      return false;

    while (!l.empty()) {
      if (l.front() != r.front())
	return false;
      l.pop();
      r.pop();
    }
    return true;
  };

  std::queue<Tp, Cont> q1(std::from_range, Range(a, a+4));
  std::queue<Tp> q2;
  q2.push_range(Range(a, a+4));
  VERIFY( eq(q1, q2) );

  std::queue<Tp, Cont> q3(std::from_range, Range(a, a+4), alloc);
  std::queue<Tp, Cont> q4(std::from_range, Range(a, a+4));
  VERIFY( eq(q3, q4) );
}

template<typename Cont>
constexpr bool
ranges_tests()
{
  using Tp = typename Cont::value_type;

  do_ranges_tests_a<test_forward_range<Tp>,  Cont>();
  do_ranges_tests_a<test_forward_sized_range<Tp>, Cont>();
  do_ranges_tests_a<
    test_sized_range_sized_sent<Tp, forward_iterator_wrapper>, Cont>();

  do_ranges_tests_a<test_input_range<Tp>, Cont>();
  do_ranges_tests_a<test_input_sized_range<Tp>, Cont>();
  do_ranges_tests_a<
    test_sized_range_sized_sent<Tp, forward_iterator_wrapper>, Cont>();

  do_ranges_tests_a<
    test_range<Tp, input_iterator_wrapper_nocopy>, Cont>();
  do_ranges_tests_a<
    test_sized_range<Tp, input_iterator_wrapper_nocopy>, Cont>();
  do_ranges_tests_a<
    test_sized_range_sized_sent<Tp, input_iterator_wrapper_nocopy>, Cont>();
  return true;
}
// TODO check list when avaialble
static_assert( ranges_tests<std::deque<int>>() );
static_assert( ranges_tests<std::deque<int, SimpleAllocator<int>>>() );

constexpr bool
push_and_pop_test()
{
  std::queue<int> a;
  a.push(2);
  a.push(4);
  VERIFY( a.front() == 2 && a.back() == 4);
  a.pop();
  VERIFY( a.front() == 4 && a.back() == 4);
  a.pop();
  VERIFY( a.empty() );
  return true;
}
static_assert( push_and_pop_test() );

constexpr bool
swap_test()
{
  std::queue<int> a,b;
  a.push(1);
  b.push(2);
  std::swap(a, b);
  VERIFY( a.front() == 2 );
  VERIFY( b.front() == 1 );
  return true;
}
static_assert( swap_test() );

constexpr bool
emplace_test()
{
  struct S
  {
    int foo;
    constexpr S(int i, int j) : foo{i + j} {}
  };

  std::queue<S> a;
  const S& s = a.emplace(196883, 1);
  VERIFY( a.size() == 1 );
  VERIFY( a.front().foo == 196884 );
  return true;
}
static_assert( emplace_test() );

constexpr bool
operator_test()
{
  std::queue<int> a, b;
  a.push(1);
  b.push(1);
  VERIFY( a == b );
  VERIFY( a <= b );
  VERIFY( a >= b );
  b.pop();
  b.push(2);
  VERIFY( a < b );
  VERIFY( !(a > b) );
  VERIFY( a <= b );
  VERIFY( !(a >= b) );
  VERIFY( a != b );
  return true;
}
static_assert( operator_test() );
