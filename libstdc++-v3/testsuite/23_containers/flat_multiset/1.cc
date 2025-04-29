// { dg-do run { target c++23 } }

#include <flat_set>
#include <deque>
#include <ranges>
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

void
test06()
{
  // PR libstdc++/118156 - flat_foo::insert_range cannot handle non-common ranges
  std::flat_multiset<int> s;
  auto r = std::views::iota(1) | std::views::take(5);
  static_assert(!std::ranges::common_range<decltype(r)>);
  s.insert_range(r);
  VERIFY( std::ranges::equal(s, (int[]){1, 2, 3, 4, 5}) );
  s.clear();
  s.insert_range(r | std::views::reverse);
  VERIFY( std::ranges::equal(s, (int[]){1, 2, 3, 4, 5}) );
}

template<typename T>
struct NoInsertRange : std::vector<T>
{
  using std::vector<T>::vector;

  template<typename It, typename R>
  void insert_range(typename std::vector<T>::const_iterator, R&&) = delete;
};

struct NoCatIterator {
  using difference_type = int;
  using value_type = int;

  NoCatIterator() : v(0) {}
  NoCatIterator(int x) : v(x) {}

  int operator*() const
  { return v; }

  NoCatIterator& operator++()
  {
    ++v;
    return *this;
  }

  NoCatIterator operator++(int)
  {
    ++v;
    return NoCatIterator(v-1);
  }

  bool operator==(const NoCatIterator& rhs) const
  { return v == rhs.v; }

private:
  int v;
};

template<>
struct std::iterator_traits<NoCatIterator> {
  using difference_type = int;
  using value_type = int;
  using iterator_concept = std::input_iterator_tag;
  // no iterator_category, happens also for common_iterator
};

void test07()
{
  std::flat_multiset<int> s;
  std::flat_multiset<int, std::less<int>, NoInsertRange<int>> s2;

  auto r = std::ranges::subrange<NoCatIterator>(1, 6);
  s.insert_range(r);
  VERIFY( std::ranges::equal(s, (int[]){1, 2, 3, 4, 5}) );
  s2.insert_range(r);
  VERIFY( std::ranges::equal(s2, (int[]){1, 2, 3, 4, 5}) );

#ifdef __SIZEOF_INT128__
  // PR libstdc++/119415 - flat_foo::insert_range cannot handle common ranges
  // on c++20 only iterators
  auto r2 = std::views::iota(__int128(1), __int128(6));
  s.clear();
  s.insert_range(r2);
  VERIFY( std::ranges::equal(s, (int[]){1, 2, 3, 4, 5}) );

  s2.clear();
  s2.insert_range(r2);
  VERIFY( std::ranges::equal(s2, (int[]){1, 2, 3, 4, 5}) );
#endif
}

void
test08()
{
  // PR libstdc++/119620 -- flat_set::emplace always constructs element on the stack
  static int copy_counter;
  struct A {
    A() { }
    A(const A&) { ++copy_counter; }
    A& operator=(const A&) { ++copy_counter; return *this; }
    auto operator<=>(const A&) const = default;
  };
  std::vector<A> v;
  v.reserve(2);
  std::flat_multiset<A> s(std::move(v));
  A a;
  s.emplace(a);
  VERIFY( copy_counter == 1 );
  s.emplace(a);
  VERIFY( copy_counter == 2 );
}

void
test09()
{
  // PR libstdc++/119427 - std::erase_if(std::flat_foo) does not work
  std::flat_multiset<int> s = {1,1,2,2,3,4,5};
  auto n = std::erase_if(s, [](int x) { return x % 2 != 0; });
  VERIFY( n == 4 );
  VERIFY( std::ranges::equal(s, (int[]){2,2,4}) );
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
  test06();
  test07();
  test08();
  test09();
}
