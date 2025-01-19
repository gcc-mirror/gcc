// { dg-do run { target c++23 } }

#include <flat_set>
#include <deque>
#include <vector>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

template<template<class> class Sequence>
void
test01()
{
  std::flat_multiset<int, std::less<int>, Sequence<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  m.insert(1);
  m.insert(2);
  m.insert(3);
  m.insert(1);
  m.insert(2);
  m.insert(3);
  m.insert(0);
  VERIFY( m.size() == 7 );
  VERIFY( std::ranges::equal(m, (int[]){0, 1, 1, 2, 2, 3, 3}) );

  m.clear();
  m.insert(m.begin(), 0);
  m.insert(m.begin(), 1);
  m.insert(m.begin(), 2);
  m.insert(m.begin(), 3);
  m.insert(m.begin(), 1);
  m.insert(m.begin(), 2);
  m.insert(m.begin(), 3);
  m.insert(m.begin(), 0);
  VERIFY( std::ranges::equal(m, (int[]){0, 0, 1, 1, 2, 2, 3, 3}) );

  m.clear();
  m = {10,10};
  VERIFY( m.size() == 2 );
  m.insert({11,12,11});
  VERIFY( m.size() == 5 );
}

void
test02()
{
  std::flat_multiset<int, std::greater<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  auto r = m.insert(1);
  VERIFY( *r == 1 );
  r = m.insert(2);
  VERIFY( *r == 2 );
  r = m.insert(3);
  VERIFY( *r == 3 );
  r = m.insert(1);
  VERIFY( *r == 1 );
  r = m.insert(2);
  VERIFY( *r == 2 );
  r = m.insert(3);
  VERIFY( *r == 3 );
  VERIFY( m.size() == 6 );
  VERIFY( std::ranges::equal(m, (int[]){3, 3, 2, 2, 1, 1}) );

  VERIFY( m.contains(3) && !m.contains(7) );
  VERIFY( m.count(3) == 2 );
}

void
test03()
{
  std::flat_set<int> m;
  m = {1, 3, 5};
  m.insert({7, 9});

  auto it = m.find(0);
  VERIFY( it == m.end() );
  it = m.find(9);
  VERIFY( it == m.end()-1 );

  const auto n = m;
  VERIFY( m == m );
  VERIFY( m == n );

  m.erase(m.begin());
  m.erase(5);
  m.erase(m.end()-2, m.end());
  VERIFY( std::ranges::equal(m, (int[]){3}) );
  VERIFY( m != n );
  VERIFY( n < m );

  m = n;
  erase_if(m, [](const auto& k) { return k < 5 || k > 5; });
  VERIFY( std::ranges::equal(m, (int[]){5}) );
}

void
test04()
{
  using vector = std::vector<int, __gnu_test::uneq_allocator<int>>;
  vector v = {1, 2, 3};
  __gnu_test::uneq_allocator<int> alloc(42);

  using flat_multiset = std::flat_multiset<int, std::less<int>, vector>;
  flat_multiset m1(alloc);
  VERIFY( std::move(m1).extract().get_allocator().get_personality() == 42 );

  flat_multiset m2(v, alloc);
  VERIFY( std::move(m2).extract().get_allocator().get_personality() == 42 );

  flat_multiset m3(std::sorted_equivalent_t{}, v, alloc);
  VERIFY( std::move(m3).extract().get_allocator().get_personality() == 42 );

  alloc = __gnu_test::uneq_allocator<int>(43);
  flat_multiset m4(m3, alloc);
  VERIFY( std::move(m4).extract().get_allocator().get_personality() == 43 );

  alloc = __gnu_test::uneq_allocator<int>(44);
  flat_multiset m5(std::move(m4), alloc);
  VERIFY( std::move(m5).extract().get_allocator().get_personality() == 44 );
}

void
test05()
{
  std::vector<int> v = {2, 3, 1, 5, 4, 3};
  std::flat_multiset<int, std::less<>, std::deque<int>> m = {std::from_range, v};
  VERIFY( std::ranges::equal(m, (int[]){1, 2, 3, 3, 4, 5}) );
}

int
main()
{
  test01<std::vector>();
  test01<std::deque>();
  test02();
  test03();
  test04();
  test05();
}
