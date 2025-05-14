// { dg-do run { target c++23 } }
// { dg-timeout-factor 2 }

#include <flat_map>
#include <deque>
#include <vector>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <tuple>

struct Gt {
  template<typename T, typename U>
  bool operator()(T const& l, U const & r) const
  { return l > r; }
};

void
test_deduction_guide()
{
  __gnu_test::test_input_range<std::pair<long, float>> r(0, 0);
  std::flat_multimap it1(r.begin(), r.begin());
  static_assert(std::is_same_v<decltype(it1), std::flat_multimap<long, float>>);
  std::flat_multimap fr1(std::from_range, r);
  static_assert(std::is_same_v<decltype(fr1), std::flat_multimap<long, float>>);

  Gt cmp;
  std::flat_multimap it2(r.begin(), r.begin(), cmp);
  static_assert(std::is_same_v<
    decltype(it2),
    std::flat_multimap<long, float, Gt>>);
  std::flat_multimap fr2(std::from_range, r, cmp);
  static_assert(std::is_same_v<
    decltype(fr2),
    std::flat_multimap<long, float, Gt>>);

  using Alloc = __gnu_test::SimpleAllocator<std::pair<const long, float>>;
  Alloc alloc;
  // No matching deduction guide
  // std::flat_multimap it3(r.begin(), r.begin(), alloc);
  std::flat_multimap fr3(std::from_range, r, alloc);
  static_assert(std::is_same_v<
     decltype(fr3),
     std::flat_multimap<long, float, std::less<long>,
		   std::vector<long, __gnu_test::SimpleAllocator<long>>,
		   std::vector<float, __gnu_test::SimpleAllocator<float>>>>);

  // No matching deduction guide
  // std::flat_multimap it4(r.begin(), r.begin(), cmp, alloc);
  std::flat_multimap fr4(std::from_range, r, cmp, alloc);
  static_assert(std::is_same_v<
     decltype(fr4),
     std::flat_multimap<long, float, Gt,
		   std::vector<long, __gnu_test::SimpleAllocator<long>>,
		   std::vector<float, __gnu_test::SimpleAllocator<float>>>>);

  __gnu_test::test_input_range<std::pair<const long, const float>> r2(0, 0);
  std::flat_multimap it5(r2.begin(), r2.begin());
  static_assert(std::is_same_v<decltype(it5), std::flat_multimap<long, float>>);
  std::flat_multimap fr5(std::from_range, r2);
  static_assert(std::is_same_v<decltype(fr5), std::flat_multimap<long, float>>);

  __gnu_test::test_input_range<std::pair<const long&, float&>> r3(0, 0);
  std::flat_multimap it6(r3.begin(), r3.begin());
  static_assert(std::is_same_v<decltype(it6), std::flat_multimap<long, float>>);
  std::flat_multimap fr6(std::from_range, r3);
  static_assert(std::is_same_v<decltype(fr6), std::flat_multimap<long, float>>);

  __gnu_test::test_input_range<std::tuple<long, float>> r4(0, 0);
  std::flat_multimap it7(r4.begin(), r4.begin());
  static_assert(std::is_same_v<decltype(it7), std::flat_multimap<long, float>>);
  std::flat_multimap fr7(std::from_range, r4);
  static_assert(std::is_same_v<decltype(fr7), std::flat_multimap<long, float>>);
}

template<template<typename> class KeyContainer, template<typename> class MappedContainer>
void
test01()
{
  std::flat_multimap<int, int, std::less<int>, KeyContainer<int>, MappedContainer<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  m.insert({1,-1});
  m.insert({2,-2});
  m.insert({3,-3});
  m.insert({1,-4});
  m.insert({2,-5});
  m.insert({3,-6});
  m.insert({0, 0});
  VERIFY( m.size() == 7 );
  VERIFY( std::ranges::equal(m.keys(), (int[]){0, 1, 1, 2, 2, 3, 3}) );
  VERIFY( std::ranges::equal(m.values(), (int[]){0, -1, -4, -2, -5, -3, -6}) );

  m.clear();
  m.insert(m.begin(), {0, 0});
  m.insert(m.begin(), {1,-1});
  m.insert(m.begin(), {2,-2});
  m.insert(m.begin(), {3,-3});
  m.insert(m.begin(), {1,-4});
  m.insert(m.begin(), {2,-5});
  m.insert(m.begin(), {3,-6});
  m.insert(m.begin(), {0,-7});
  VERIFY( std::ranges::equal(m.keys(), (int[]){0, 0, 1, 1, 2, 2, 3, 3}) );
  VERIFY( std::ranges::equal(m.values(), (int[]){-7, 0, -4, -1, -5, -2, -6, -3}) );

  m.clear();
  m = {{10,0},{10,1}};
  VERIFY( m.size() == 2 );
  m.insert({{11,2},{12,3},{11,4}});
  VERIFY( m.size() == 5 );
  VERIFY( m.end()[-1] == std::pair(12,3) );
}

void
test02()
{
  std::flat_multimap<int, int, std::greater<int>> m;
  static_assert( std::ranges::random_access_range<decltype(m)> );

  auto r = m.insert({1,-1});
  VERIFY( r->first == 1 && r->second == -1 );
  r = m.insert({2,-2});
  VERIFY( r->first == 2 && r->second == -2 );
  r = m.insert({3,-3});
  VERIFY( r->first == 3 && r->second == -3 );
  r = m.insert({1,-4});
  VERIFY( r->first == 1 && r->second == -4 );
  r = m.insert({2,-5});
  VERIFY( r->first == 2 && r->second == -5 );
  r = m.insert({3,-6});
  VERIFY( r->first == 3 && r->second == -6 );
  VERIFY( m.size() == 6 );
  VERIFY( std::ranges::equal(m.keys(), (int[]){3, 3, 2, 2, 1, 1}) );
  VERIFY( std::ranges::equal(m.values(), (int[]){-3, -6, -2, -5, -1, -4}) );

  VERIFY( m.contains(3) && !m.contains(7) );
  VERIFY( m.count(3) == 2 );
}

void
test03()
{
  std::flat_multimap<int, int> m;
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

  using flat_multimap = std::flat_multimap<int, int, std::less<int>, vector, vector>;
  flat_multimap m1(alloc);
  VERIFY( m1.keys().get_allocator().get_personality() == 42 );
  VERIFY( m1.values().get_allocator().get_personality() == 42 );

  flat_multimap m2(v1, v2, alloc);
  VERIFY( m2.keys().get_allocator().get_personality() == 42 );
  VERIFY( m2.values().get_allocator().get_personality() == 42 );

  flat_multimap m3(std::sorted_equivalent_t{}, v1, v2, alloc);
  VERIFY( m2.keys().get_allocator().get_personality() == 42 );
  VERIFY( m2.values().get_allocator().get_personality() == 42 );

  alloc = __gnu_test::uneq_allocator<int>(43);
  flat_multimap m4(m2, alloc);
  VERIFY( m4.keys().get_allocator().get_personality() == 43 );
  VERIFY( m4.values().get_allocator().get_personality() == 43 );

  alloc = __gnu_test::uneq_allocator<int>(44);
  flat_multimap m5(std::move(m4), alloc);
  VERIFY( m5.keys().get_allocator().get_personality() == 44 );
  VERIFY( m5.values().get_allocator().get_personality() == 44 );
}

void
test05()
{
  std::vector<std::pair<int, int>> v = {{2, -2}, {3,-3}, {1,-1}, {5,-5}, {4,-4}, {3,3}};
  std::flat_multimap<int, int> m = {std::from_range, v};
  VERIFY( std::ranges::equal(m | std::views::keys, (int[]){1, 2, 3, 3, 4, 5}) );
  VERIFY( std::ranges::equal(m | std::views::values, (int[]){-1, -2, -3, 3, -4, -5}) );
}
void
test06()
{
  // PR libstdc++/118156 - flat_foo::insert_range cannot handle non-common ranges
  std::flat_multimap<int, int> m;
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

void
test07()
{
  // PR libstdc++/119427 - std::erase_if(std::flat_foo) does not work
  std::flat_multimap<int, int> m = {std::pair{1, 2}, {3, 4}, {3, 3}, {5, 6}, {6, 6}};
  auto n = std::erase_if(m, [](auto x) { auto [k,v] = x; return k == 1 || v == 6; });
  VERIFY( n == 3 );
  VERIFY( std::ranges::equal(m, (std::pair<int,int>[]){{3,4},{3,3}}) );
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
  test07();
}
