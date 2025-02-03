// { dg-do run { target c++23 } }

#include <flat_map>

#if __cpp_lib_flat_map != 202207L
# error "Feature-test macro __cpp_lib_flat_map has wrong value in <flat_map>"
#endif

#include <deque>
#include <ranges>
#include <vector>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

template<template<typename> class KeyContainer, template<typename> class MappedContainer>
void
test01()
{
  std::flat_map<int, int, std::less<int>, KeyContainer<int>, MappedContainer<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  m.insert({1,-1});
  m.insert({2,-2});
  m.insert({3,-3});
  m.insert({1,-4});
  m.insert({2,-5});
  m.insert({3,-6});
  m.insert({0, 0});
  VERIFY( m.size() == 4 );
  VERIFY( std::ranges::equal(m.keys(), (int[]){0, 1, 2, 3}) );
  VERIFY( std::ranges::equal(m.values(), (int[]){0, -1, -2, -3}) );

  for (int i = 0; i < 4; i++)
    {
      m.clear();

      int j = i;
      m.insert(m.end(), {j,-j});
      j = (j + 1) % 4;
      m.insert(m.end(), {j,-j});
      j = (j + 1) % 4;
      m.insert(m.end(), {j,-j});
      j = (j + 1) % 4;
      m.insert(m.end(), {j,-j});

      m.insert(m.begin() + i, {1,-4});
      m.insert(m.begin() + i, {2,-5});
      m.insert(m.begin() + i, {3,-6});
      m.insert(m.begin() + i, {0,-7});
      VERIFY( std::ranges::equal(m.keys(), (int[]){0, 1, 2, 3}) );
      VERIFY( std::ranges::equal(m.values(), (int[]){0, -1, -2, -3}) );
    }

  m.clear();
  m = {{10,0},{10,1}};
  VERIFY( m.size() == 1 );
  m.insert({{11,2},{12,3},{11,4}});
  VERIFY( m.size() == 3 );
  VERIFY( m[10] == 0 );
  VERIFY( m[11] == 2 );
  VERIFY( m[12] == 3 );
  m[20] = 42;
  VERIFY( m[20] == 42 );
  VERIFY( m.end()[-1] == std::pair(20,42) );
}

void
test02()
{
  std::flat_map<int, int, std::greater<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  auto r = m.insert({1,-1});
  VERIFY( r.first->first == 1 && r.first->second == -1 && r.second );
  r = m.insert({2,-2});
  VERIFY( r.first->first == 2 && r.first->second == -2 && r.second );
  r = m.insert({3,-3});
  VERIFY( r.first->first == 3 && r.first->second == -3 && r.second );
  r = m.insert({1,-4});
  VERIFY( r.first->first == 1 && r.first->second == -1 && !r.second );
  r = m.insert({2,-5});
  VERIFY( r.first->first == 2 && r.first->second == -2 && !r.second );
  r = m.insert({3,-6});
  VERIFY( r.first->first == 3 && r.first->second == -3 && !r.second );
  r = m.insert_or_assign(0, 0);
  VERIFY( r.first->first == 0 && r.first->second == 0 && r.second );
  r = m.insert_or_assign(0, 1);
  VERIFY( r.first->first == 0 && r.first->second == 1 && !r.second );
  VERIFY( *m.insert_or_assign(m.end(), 0, 2) == std::pair(0, 2) );
  VERIFY( m.size() == 4 );
  VERIFY( std::ranges::equal(m.keys(), (int[]){3, 2, 1, 0}) );
  VERIFY( std::ranges::equal(m.values(), (int[]){-3, -2, -1, 2}) );

  VERIFY( m.contains(3) && !m.contains(7) );
  VERIFY( m.count(3) == 1 );
}

void
test03()
{
  std::flat_map<int, int> m;
  m = {std::pair(1, 2), {3, 4}, {5, 6}};
  m.insert({std::pair(7, 8), {9, 10}});

  auto it = m.find(0);
  VERIFY( it == m.end() );
  it = m.find(9);
  VERIFY( it->second == 10 );

  const auto n = m;
  VERIFY( m == m );
  VERIFY( m == n );

  m.erase(m.begin());
  m.erase(5);
  m.erase(m.end()-2, m.end());
  VERIFY( std::ranges::equal(m, (std::pair<int, int>[]){{3, 4}}) );
  VERIFY( m != n );
  VERIFY( n < m );

  m = n;
  erase_if(m, [](const auto& x) { auto [k, v] = x; return k < 5 || k > 5; });
  VERIFY( std::ranges::equal(m, (std::pair<int, int>[]){{5, 6}}) );
}

void
test04()
{
  using vector = std::vector<int, __gnu_test::uneq_allocator<int>>;
  vector v1 = {1, 2, 3};
  vector v2 = {4, 5, 6};
  __gnu_test::uneq_allocator<int> alloc(42);

  using flat_map = std::flat_map<int, int, std::less<int>, vector, vector>;
  flat_map m1(alloc);
  VERIFY( m1.keys().get_allocator().get_personality() == 42 );
  VERIFY( m1.values().get_allocator().get_personality() == 42 );

  flat_map m2(v1, v2, alloc);
  VERIFY( m2.keys().get_allocator().get_personality() == 42 );
  VERIFY( m2.values().get_allocator().get_personality() == 42 );

  flat_map m3(std::sorted_unique_t{}, v1, v2, alloc);
  VERIFY( m2.keys().get_allocator().get_personality() == 42 );
  VERIFY( m2.values().get_allocator().get_personality() == 42 );

  alloc = __gnu_test::uneq_allocator<int>(43);
  flat_map m4(m3, alloc);
  VERIFY( m4.keys().get_allocator().get_personality() == 43 );
  VERIFY( m4.values().get_allocator().get_personality() == 43 );

  alloc = __gnu_test::uneq_allocator<int>(44);
  flat_map m5(std::move(m4), alloc);
  VERIFY( m5.keys().get_allocator().get_personality() == 44 );
  VERIFY( m5.values().get_allocator().get_personality() == 44 );
}

void
test05()
{
  std::vector<std::pair<int, int>> v = {{2, -2}, {3,-3}, {1,-1}, {5,-5}, {4,-4}};
  std::flat_map<int, int> m = {std::from_range, v};
  VERIFY( std::ranges::equal(m | std::views::keys, (int[]){1, 2, 3, 4, 5}) );
  VERIFY( std::ranges::equal(m | std::views::values, (int[]){-1, -2, -3, -4, -5}) );
}

void
test06()
{
  // PR libstdc++/118156 - flat_foo::insert_range cannot handle non-common ranges
  std::flat_map<int, int> m;
  auto r = std::views::zip(std::views::iota(1), std::views::iota(2)) | std::views::take(5);
  static_assert(!std::ranges::common_range<decltype(r)>);
  m.insert_range(r);
  VERIFY( std::ranges::equal(m | std::views::keys, (int[]){1, 2, 3, 4, 5}) );
  VERIFY( std::ranges::equal(m | std::views::values, (int[]){2, 3, 4, 5, 6}) );
  m.clear();
  m.insert_range(r | std::views::reverse);
  VERIFY( std::ranges::equal(m | std::views::keys, (int[]){1, 2, 3, 4, 5}) );
  VERIFY( std::ranges::equal(m | std::views::values, (int[]){2, 3, 4, 5, 6}) );
}

int
main()
{
  test01<std::vector, std::vector>();
  test01<std::deque, std::deque>();
  test01<std::vector, std::deque>();
  test01<std::deque, std::vector>();
  test02();
  test03();
  test04();
  test05();
  test06();
}
