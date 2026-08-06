// { dg-do compile { target c++26 } }

#include <deque>

#ifndef __cpp_lib_constexpr_deque
#error "Feature test macro for constexpr deque is missing in <deque>"
#elif __cpp_lib_constexpr_deque != 202502L
# error "Feature test macro for constexpr deque has wrong value in <deque>"
#endif

#include <ranges>
#include <numeric>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

using namespace __gnu_test;

template<typename Alloc>
constexpr bool
ctor_tests()
{
  Alloc alloc;

  std::deque<int, Alloc> dq2(alloc);
  std::deque<int, Alloc> dq3(4, alloc);
  VERIFY( dq3.front() == 0 );
  VERIFY( dq3.size() == 4 );
  std::deque<int, Alloc> dq4(4, 5, alloc);
  VERIFY( dq4.front() == 5 );
  VERIFY( dq4.size() == 4 );

  auto rg = {2, 3, 5, 7};
  std::deque<int> dq5(rg.begin(), rg.end());
  VERIFY( dq5.front() == 2 );
  VERIFY( dq5.back() == 7 );
  VERIFY( dq5.size() == 4 );

  std::deque<int> dq6(std::from_range, rg);
  VERIFY( dq6 == dq5 );

  std::deque<int> dq1{1, 2, 3};
  std::deque<int> dq7(dq1);
  VERIFY( dq7 == dq1 );
  std::deque<int>dq8(std::move(dq1));
  VERIFY( dq8 == dq7 );

  dq2.insert_range(dq2.begin(), rg);
  std::deque<int, Alloc> dq9(dq2, alloc);
  std::deque<int, Alloc> dq10(std::move(dq2), alloc);
  VERIFY( dq9 == dq10 );
  std::deque<int, Alloc> dq11(rg, alloc);
  VERIFY( dq11 == dq10 );

  return true;
}
static_assert( ctor_tests<std::allocator<int>>() );
static_assert( ctor_tests<SimpleAllocator<int>>() );

constexpr bool
insert_tests()
{
  std::deque<int> dq1 {};

  auto rg = {1, 2, 3, 4, 5};

  dq1.insert(dq1.begin() , 1);
  dq1.insert(dq1.end(), 2);
  VERIFY( dq1.size() == 2 );
  VERIFY( dq1.front() == 1 );
  VERIFY( dq1.back() == 2 );

  dq1.insert(dq1.end(), 1, 3);

  dq1.insert(dq1.end(), rg.begin() + 3, rg.end());

  VERIFY( dq1[0] == 1 );
  VERIFY( dq1[1] == 2 );
  VERIFY( dq1[2] == 3 );
  VERIFY( dq1[3] == 4 );
  VERIFY( dq1[4] == 5 );
  dq1.clear();

  dq1.insert(dq1.begin(), rg.begin(), rg.end());
  VERIFY( dq1[0] == 1 );
  VERIFY( dq1[1] == 2 );
  VERIFY( dq1[2] == 3 );
  VERIFY( dq1[3] == 4 );
  VERIFY( dq1[4] == 5 );

  dq1.insert_range(dq1.end(), rg);
  VERIFY( dq1[5] == 1 );
  VERIFY( dq1[6] == 2 );
  VERIFY( dq1[7] == 3 );
  VERIFY( dq1[8] == 4 );
  VERIFY( dq1[9] == 5 );

  std::deque<int>dq2 {2, 3, 5, 7};
  dq2.erase(dq2.begin());
  VERIFY( dq2.size() == 3 );
  dq2.clear();
  VERIFY( dq2.size() == 0);
  VERIFY( dq2.empty() );

  std::deque<int> dq3, dq4;
  dq3.insert_range(dq3.begin(), rg);
  dq4.append_range(rg);
  VERIFY( dq3 == dq4 );
  dq3.erase(dq3.begin(), dq3.end());
  dq3.prepend_range(rg);
  VERIFY( dq3 == dq4 );

  struct S {
    int foo;
    constexpr S (int i, int j) : foo{i + j} {}
  };
  std::deque<S> dq5 {};
  dq5.emplace(dq5.end(), 0, 1);
  const S& s1 = dq5.emplace_back(1, 1);
  const S& s2 = dq5.emplace_front(2, 1);
  VERIFY( dq5.front().foo == 3 );
  VERIFY( dq5.back().foo == 2 );
  VERIFY( dq5[1].foo == 1 );

  std::deque<int> dq6 {2, 3};
  dq6.push_front(1);
  dq6.push_back(4);
  VERIFY( dq6.front() == 1);
  VERIFY( dq6.back() == 4 );
  dq6.pop_front();
  dq6.pop_back();

  std::deque<int> dq7 {1, 2};
  dq7.resize(4);
  VERIFY( dq7.back() == 0 );
  dq7.resize(2);
  VERIFY( dq7.front() == 1 );
  VERIFY( dq7.back() == 2 );
  dq7.clear();
  dq7.resize(2);
  VERIFY( dq7.front() == 0 );
  VERIFY( dq7.back() == 0 );

  std::deque<int> dq8 {1, 4};
  dq8.swap(dq6);
  VERIFY( dq6.front() == 1 );
  VERIFY( dq6.back() == 4 );
  VERIFY( dq8.front() == 2 );
  VERIFY( dq8.back() == 3 );

  return true;
}

static_assert( insert_tests() );

constexpr bool
iterators_tests()
{
  std::deque<int> dq0 {};
  VERIFY( dq0.begin() == dq0.end() );
  dq0.resize(1);
  VERIFY( dq0.begin() != dq0.end() );
  dq0.resize(2);
  VERIFY( dq0.begin() != dq0.end() );
  VERIFY( dq0.cbegin() == dq0.begin() );
  VERIFY( dq0.crbegin() == dq0.rbegin() );
  VERIFY( dq0.cend() == dq0.end() );
  VERIFY( dq0.crend() == dq0.rend() );

  auto it = dq0.begin();
  VERIFY( it[0] == 0 );
  VERIFY( &*it == &dq0.front() );
  VERIFY( &it[1] == &dq0[1] );
  VERIFY( it++ == dq0.begin() );
  VERIFY( ++it == dq0.end() );
  VERIFY( (it - 2) == dq0.begin() );
  VERIFY( (it - dq0.begin()) == 2 );
  it -= 2;
  it += 1;
  VERIFY( (it + 1) == dq0.end() );
  VERIFY( (1 + it) == dq0.end() );
  it = it + 1;
  auto it2 = dq0.begin();
  std::swap(it, it2);
  VERIFY( it == dq0.begin() );
  VERIFY( it2 == dq0.end() );

  auto rit = dq0.rbegin();
  VERIFY( rit[0] == 0 );
  VERIFY( &*rit == &dq0.back() );
  VERIFY( &rit[1] == &dq0[0] );
  VERIFY( rit++ == dq0.rbegin() );
  VERIFY( ++rit == dq0.rend() );
  VERIFY( (rit - 2) == dq0.rbegin() );
  VERIFY( (rit - dq0.rbegin()) == 2 );
  rit -= 2;
  rit += 1;
  VERIFY( (rit + 1) == dq0.rend() );
  VERIFY( (1 + rit) == dq0.rend() );
  rit = rit + 1;
  auto rit2 = dq0.rbegin();
  std::swap(rit, rit2);
  VERIFY( rit == dq0.rbegin() );
  VERIFY( rit2 == dq0.rend() );

  return true;
}
static_assert( iterators_tests() );

constexpr bool
capacity_tests()
{
  std::deque<int> dq0 {};
  VERIFY( dq0.empty() );
  VERIFY( dq0.max_size() );
  dq0.push_front(0);
  VERIFY( dq0.size() == 1 );
  dq0.erase(dq0.begin());
  dq0.shrink_to_fit();

  return true;
}
static_assert( capacity_tests() );

constexpr bool
nonmember_tests()
{
  std::deque<int> dq0 {0, 1};
  std::deque<int> dq1 {0, 1};

  VERIFY( (dq0 == dq1) == true );
  VERIFY( (dq0 != dq1) == false );
  VERIFY( (dq0 <= dq1) == true );
  VERIFY( (dq0 >= dq1) == true );
  VERIFY( (dq0  < dq1) == false );
  VERIFY( (dq0  > dq1) == false );
  VERIFY( (dq0 <=> dq1) == 0 );
  VERIFY( (dq0 <=> dq1) <= 0 );
  VERIFY( (dq0 <=> dq1) >= 0 );

  std::deque<int> dq2 {2, 4};
  std::swap(dq1, dq2);

  VERIFY( (dq0 == dq1) == false );
  VERIFY( (dq0 != dq1) == true );
  VERIFY( (dq0 <= dq1) == true );
  VERIFY( (dq0 >= dq1) == false );
  VERIFY( (dq0  < dq1) == true );
  VERIFY( (dq0  > dq1) == false );
  VERIFY( (dq0 <=> dq1) != 0 );
  VERIFY( (dq0 <=> dq1) <= 0 );
  VERIFY( (dq0 <=> dq1)  < 0 );

  std::erase(dq0, 0);
  VERIFY( dq0.front() == 1 );

  std::erase_if(dq1, [](int x) { return x % 2 == 0; });
  VERIFY( dq1.empty() );
  return true;
}
static_assert( nonmember_tests() );

template<typename Range>
constexpr bool
ranges_test()
{
  using Tp = std::ranges::range_value_t<Range>;
  Tp a[] {0, 1, 2, 3, 4,
   5, 6, 7, 8, 9};

  // Constructor tests
  auto dq1 = std::deque<Tp>(std::from_range, Range(a, a+5));
  VERIFY( dq1[0] == 0 );
  VERIFY( dq1[1] == 1 );
  VERIFY( dq1[2] == 2 );
  VERIFY( dq1[3] == 3 );
  VERIFY( dq1[4] == 4 );

  // Insert tests
  std::deque<Tp> dq2;
  dq2.insert_range(dq2.begin(), Range(a, a+5));
  VERIFY( dq2[0] == 0 );
  VERIFY( dq2[1] == 1 );
  VERIFY( dq2[2] == 2 );
  VERIFY( dq2[3] == 3 );
  VERIFY( dq2[4] == 4 );

  std::deque<Tp> dq3;
  dq3.assign_range(Range(a, a+5));
  VERIFY( dq2 == dq3 );

  dq2.insert_range(dq2.end(), Range(a+5, a+10));
  VERIFY( dq2[5] == 5 );
  VERIFY( dq2[6] == 6 );
  VERIFY( dq2[7] == 7 );
  VERIFY( dq2[8] == 8 );
  VERIFY( dq2[9] == 9 );

  std::deque<Tp> dq4;
  dq4.prepend_range(Range(a, a+5));
  dq4.append_range(Range(a+5, a+10));
  VERIFY( dq2 == dq4 );

  return true;
}

static_assert( ranges_test<test_forward_range<int>>() );
static_assert( ranges_test<test_forward_sized_range<int>>() );
static_assert( ranges_test<test_sized_range_sized_sent<int, forward_iterator_wrapper>>() );

static_assert( ranges_test<test_input_range<int>>() );
static_assert( ranges_test<test_input_sized_range<int>>() );
static_assert( ranges_test<test_sized_range_sized_sent<int, input_iterator_wrapper>>() );

static_assert( ranges_test<test_range<int, input_iterator_wrapper_nocopy>>() );
static_assert( ranges_test<test_sized_range<int, input_iterator_wrapper_nocopy>>() );
static_assert( ranges_test<test_sized_range_sized_sent<int, input_iterator_wrapper_nocopy>>() );

static_assert( ranges_test<test_forward_range<short>>() );
static_assert( ranges_test<test_input_range<short>>() );

// Not lvalue-convertible to int
struct C {
  constexpr C(int v) : val(v) { }
  constexpr operator int() && { return val; }

  friend bool operator==(C c, C b) = default;
  friend constexpr bool operator==(C c, int b)
  { return b == c.val; }

  int val;
};
using rvalue_input_range = test_range<C, input_iterator_wrapper_rval>;
static_assert( ranges_test<rvalue_input_range>() );
