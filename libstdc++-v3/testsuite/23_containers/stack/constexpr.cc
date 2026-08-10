// { dg-do compile { target c++26 } }

#include <stack>

#ifndef __cpp_lib_constexpr_stack
# error "Feature test macro for __cpp_lib_constexpr_stack is missing in <stack>"
#elif __cpp_lib_constexpr_stack != 202502L
# error "Feature test macro for __cpp_lib_constexpr_stack has wrong value in <stack>"
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

  auto es = [] (std::stack<Tp, Cont> l, std::span<Tp> r) {
    if (l.size() != r.size())
      return false;

    std::vector<Tp> s(r.begin(), r.end());
    for (size_t i = s.size(); i > 0; --i) {
      if (s[i-1] != l.top())
	return false;
      l.pop();
    }
    return true;
  };

  Cont c0;
  Alloc alloc0;

  std::stack<Tp, Cont> s1(c0);
  VERIFY( s1.size() == 0 && s1.empty() );
  s1.push(1);
  s1.push(2);
  VERIFY( s1.size() == 2 );

  Cont c1{1, 2};
  std::stack<Tp, Cont> s2(c1);
  VERIFY ( s2 == s1 );
  std::stack<Tp, Cont> s3(std::move(c1));
  VERIFY ( s3 == s1 );

  std::stack<Tp, Cont> s4(s1);
  std::stack<Tp, Cont> s5(std::move(s1));
  VERIFY ( s4 == s5 );

  Tp rg[4] = {2, 3, 5, 7};
  std::stack<Tp, Cont> s6(std::begin(rg), std::end(rg));
  VERIFY ( es(s6, rg) );

  VERIFY( s6.size() == std::size(rg));
  VERIFY( s6.top() == 7 );
  s6.pop();
  VERIFY( s6.top() == 5 );
  s6.pop();
  VERIFY( s6.top() == 3 );
  s6.pop();
  VERIFY( s6.top() == 2 );
  s6.pop();

  std::stack<Tp, Cont> s7(alloc0);
  s7.push(1);
  s7.push(2);
  VERIFY( s7.size() == 2 );

  Cont c2{1, 2};
  std::stack<Tp, Cont> s8(c2, alloc0);
  VERIFY( s8 == s7 );
  std::stack<Tp, Cont> s9(std::move(c2), alloc0);
  VERIFY( s9 == s7 );
  VERIFY( c2.empty() );

  std::stack<Tp, Cont> s10(s7, alloc0);
  VERIFY( s10 == s7 );
  VERIFY( s10.size() == s7.size() );
  VERIFY( s10.top() == s7.top() );

  std::stack<Tp, Cont> s11(std::move(s7), alloc0);
  VERIFY( s11 == s10 );
  VERIFY( s11.size() == s10.size() );
  VERIFY( s7.empty() );

  std::stack<Tp, Cont> s12(std::begin(rg), std::end(rg), alloc0);
  VERIFY ( es(s12, rg) );
  VERIFY( s12.size() == std::size(rg));
  VERIFY( s12.top() == 7 );
  s12.pop();
  VERIFY( s12.top() == 5 );
  s12.pop();
  VERIFY( s12.top() == 3 );
  s12.pop();
  VERIFY( s12.top() == 2 );
  s12.pop();

  std::stack<Tp, Cont> s13(std::from_range, rg);
  VERIFY( es(s13, rg) );
  std::stack<Tp, Cont> s14(std::from_range, rg, alloc0);
  VERIFY( es(s14, rg) );

  return true;
}
static_assert( ctor_tests<std::vector<int>>() );
static_assert( ctor_tests<std::vector<int, SimpleAllocator<int>>>() );
static_assert( ctor_tests<std::deque<int>>() );
static_assert( ctor_tests<std::deque<int, SimpleAllocator<int>>>() );

template<typename Range, typename Cont>
constexpr void
do_ranges_tests_a()
{
  using Tp = typename Cont::value_type;
  typename Cont::allocator_type alloc;
  Tp a[] {2, 3, 5, 7};

  auto es = [&] (auto l, auto r) {
    if (l.size() != r.size())
      return false;

    while (!l.empty()) {
      if (l.top() != r.top())
	return false;
      l.pop();
      r.pop();
    }
    return true;
  };

  std::stack<Tp, Cont> s1(std::from_range, Range(a, a+4));
  std::stack<Tp> s2;
  s2.push_range(Range(a, a+4));
  VERIFY( es(s1, s2) );

  std::stack<Tp, Cont> s3(std::from_range, Range(a, a+4), alloc);
  std::stack<Tp, Cont> s4(std::from_range, Range(a, a+4));
  VERIFY( es(s3, s4) );
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
static_assert( ranges_tests<std::vector<int>>() );
static_assert( ranges_tests<std::vector<int, SimpleAllocator<int>>>() );

constexpr bool
push_and_pop_test()
{
  std::stack<int> a;
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

constexpr bool
swap_test()
{
  std::stack<int> a,b;
  a.push(1);
  b.push(2);
  std::swap(a, b);
  VERIFY( a.top() == 2 );
  VERIFY( b.top() == 1 );
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

  std::stack<S> a;
  const S& s = a.emplace(196883, 1);
  VERIFY( a.size() == 1 );
  VERIFY( a.top().foo == 196884 );
  return true;
}
static_assert( emplace_test() );

constexpr bool
operator_test()
{
  std::stack<int> a, b;
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
