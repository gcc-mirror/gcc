// { dg-do run { target c++23 } }

#include <flat_set>

#if __cpp_lib_flat_set != 202207L
# error "Feature-test macro __cpp_lib_flat_set has wrong value in <flat_set>"
#endif

#include <deque>
#include <vector>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

template<template<typename> class KeyContainer>
void
test01()
{
  std::flat_set<int, std::less<int>, KeyContainer<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  m.insert(1);
  m.insert(2);
  m.insert(3);
  m.insert(1);
  m.insert(2);
  m.insert(3);
  m.insert(0);
  VERIFY( m.size() == 4 );
  VERIFY( std::ranges::equal(m, (int[]){0, 1, 2, 3}) );

  for (int i = 0; i < 4; i++)
    {
      m.clear();
      m.insert(m.end(), (i + 0) % 4);
      m.insert(m.end(), (i + 1) % 4);
      m.insert(m.end(), (i + 2) % 4);
      m.insert(m.end(), (i + 3) % 4);
      m.insert(m.begin() + i, 1);
      m.insert(m.begin() + i, 2);
      m.insert(m.begin() + i, 3);
      m.insert(m.begin() + i, 0);
      VERIFY( std::ranges::equal(m, (int[]){0, 1, 2, 3}) );
    }

  m.clear();
  m = {10,10};
  VERIFY( m.size() == 1 );
  m.insert({11, 12, 11});
  VERIFY( m.size() == 3 );
  VERIFY( m.end()[-1] == 12 );

  auto m_copy = m;
  VERIFY( m_copy == m );
  m_copy.operator=({12, 11, 10});
  VERIFY( m_copy == m );
}

void
test02()
{
  std::flat_set<int, std::greater<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  auto r = m.insert(1);
  VERIFY( *r.first == 1 && r.second );
  r = m.insert(2);
  VERIFY( *r.first == 2 && r.second );
  r = m.insert(3);
  VERIFY( *r.first == 3 && r.second );
  r = m.insert(1);
  VERIFY( *r.first == 1 && !r.second );
  r = m.insert(2);
  VERIFY( *r.first == 2 && !r.second );
  r = m.insert(3);
  VERIFY( *r.first == 3 && !r.second );
  m.insert(0);
  VERIFY( m.size() == 4 );
  VERIFY( std::ranges::equal(m, (int[]){3, 2, 1, 0}) );

  VERIFY( m.contains(3) && !m.contains(7) );
  VERIFY( m.count(3) == 1 );
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

  using flat_set = std::flat_set<int, std::less<int>, vector>;
  flat_set m1(alloc);
  VERIFY( std::move(m1).extract().get_allocator().get_personality() == 42 );

  flat_set m2(v, alloc);
  VERIFY( std::move(m2).extract().get_allocator().get_personality() == 42 );

  flat_set m3(std::sorted_unique_t{}, v, alloc);
  VERIFY( std::move(m3).extract().get_allocator().get_personality() == 42 );

  alloc = __gnu_test::uneq_allocator<int>(43);
  flat_set m4(m3, alloc);
  VERIFY( std::move(m4).extract().get_allocator().get_personality() == 43 );

  alloc = __gnu_test::uneq_allocator<int>(44);
  flat_set m5(std::move(m4), alloc);
  VERIFY( std::move(m5).extract().get_allocator().get_personality() == 44 );
}

void
test05()
{
  std::vector<int> v = {2, 3, 1, 5, 4};
  std::flat_set<int, std::less<>, std::deque<int>> m = {std::from_range, v};
  VERIFY( std::ranges::equal(m, (int[]){1, 2, 3, 4, 5}) );
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
