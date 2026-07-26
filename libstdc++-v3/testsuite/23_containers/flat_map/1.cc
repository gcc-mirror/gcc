// { dg-do run { target c++23 } }
// { dg-timeout-factor 2 }

#include <flat_map>

#if __cpp_lib_flat_map != 202511L
# error "Feature-test macro __cpp_lib_flat_map has wrong value in <flat_map>"
#endif

#include <deque>
#include <ranges>
#include <vector>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <string>
#include <tuple>

struct Gt {
  template<typename T, typename U>
  constexpr bool operator()(T const& l, U const & r) const
  { return l > r; }
};

void
test_deduction_guide()
{
  __gnu_test::test_input_range<std::pair<long, float>> r(0, 0);
  std::flat_map it1(r.begin(), r.begin());
  static_assert(std::is_same_v<decltype(it1), std::flat_map<long, float>>);
  std::flat_map fr1(std::from_range, r);
  static_assert(std::is_same_v<decltype(fr1), std::flat_map<long, float>>);

  Gt cmp;
  std::flat_map it2(r.begin(), r.begin(), cmp);
  static_assert(std::is_same_v<decltype(it2), std::flat_map<long, float, Gt>>);
  std::flat_map fr2(std::from_range, r, cmp);
  static_assert(std::is_same_v<decltype(fr2), std::flat_map<long, float, Gt>>);

  using Alloc = __gnu_test::SimpleAllocator<std::pair<const long, float>>;
  Alloc alloc;
  // No matching deduction guide
  // std::flat_map it3(r.begin(), r.begin(), alloc);
  std::flat_map fr3(std::from_range, r, alloc);
  static_assert(std::is_same_v<
     decltype(fr3),
     std::flat_map<long, float, std::less<long>,
		   std::vector<long, __gnu_test::SimpleAllocator<long>>,
		   std::vector<float, __gnu_test::SimpleAllocator<float>>>>);

  // No matching deduction guide
  // std::flat_map it4(r.begin(), r.begin(), cmp, alloc);
  std::flat_map fr4(std::from_range, r, cmp, alloc);
  static_assert(std::is_same_v<
     decltype(fr4),
     std::flat_map<long, float, Gt,
		   std::vector<long, __gnu_test::SimpleAllocator<long>>,
		   std::vector<float, __gnu_test::SimpleAllocator<float>>>>);

  __gnu_test::test_input_range<std::pair<const long, const float>> r2(0, 0);
  std::flat_map it5(r2.begin(), r2.begin());
  static_assert(std::is_same_v<decltype(it5), std::flat_map<long, float>>);
  std::flat_map fr5(std::from_range, r2);
  static_assert(std::is_same_v<decltype(fr5), std::flat_map<long, float>>);

  __gnu_test::test_input_range<std::pair<const long&, float&>> r3(0, 0);
  std::flat_map it6(r3.begin(), r3.begin());
  static_assert(std::is_same_v<decltype(it6), std::flat_map<long, float>>);
  std::flat_map fr6(std::from_range, r3);
  static_assert(std::is_same_v<decltype(fr6), std::flat_map<long, float>>);

  __gnu_test::test_input_range<std::tuple<long, float>> r4(0, 0);
  std::flat_map it7(r4.begin(), r4.begin());
  static_assert(std::is_same_v<decltype(it7), std::flat_map<long, float>>);
  std::flat_map fr7(std::from_range, r4);
  static_assert(std::is_same_v<decltype(fr7), std::flat_map<long, float>>);
}

template<template<typename> class KeyContainer, template<typename> class MappedContainer>
constexpr
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

constexpr
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

constexpr
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

constexpr
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

constexpr
void
test05()
{
  std::vector<std::pair<int, int>> v = {{2, -2}, {3,-3}, {1,-1}, {5,-5}, {4,-4}};
  std::flat_map<int, int> m = {std::from_range, v};
  VERIFY( std::ranges::equal(m | std::views::keys, (int[]){1, 2, 3, 4, 5}) );
  VERIFY( std::ranges::equal(m | std::views::values, (int[]){-1, -2, -3, -4, -5}) );
}

constexpr
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

constexpr
void
test07()
{
  // PR libstdc++/119427 - std::erase_if(std::flat_foo) does not work
  // PR libstdc++/120465 - erase_if for flat_map calls predicate with incorrect type
  std::flat_map<int, int> m = {std::pair{1, 2}, {3, 4}, {5, 6}};
  auto n = std::erase_if(m, [](auto x) { return x.first == 1 || x.second == 6; });
  VERIFY( n == 2 );
  VERIFY( std::ranges::equal(m, (std::pair<int,int>[]){{3,4}}) );
}

constexpr
void
test08()
{
  // PR libstdc++/120432 - flat_map operator[] is broken for const lvalue keys
  std::flat_map<int, int> m;
  const int k = 42;
  m[k] = 0;
}

constexpr
void
test09()
{
  // PR libstdc++/122921 - The value_type of flat_map's iterator should be
  // pair<Key, T> instead of pair<const Key, T>
  using type = std::flat_map<int, int>;
  using value_type = std::ranges::range_value_t<type>;
  using value_type = type::value_type;
  using value_type = std::pair<int, int>;
}

constexpr
void
test10()
{
  // PR libstdc++/125374 - flat_map unconditionally moves from lvalue keys in
  // _M_try_emplace
  std::flat_map<std::string, int, std::less<>> m;
  std::string k = "hello";
  m[k] = 1;
  VERIFY (k == "hello");
  k = "world";
  m.try_emplace(k, 2);
  VERIFY (k == "world");
}

template<typename T>
struct throwing_vector : std::vector<T>
{
  static inline bool throw_on_move = false;

  throwing_vector() = default;
  throwing_vector(const throwing_vector&) = default;
  throwing_vector& operator=(const throwing_vector&) = default;

  throwing_vector(throwing_vector&& other)
  : std::vector<T>(std::move(other))
  {
    if (throw_on_move)
      throw std::runtime_error("move ctor");
  }

  throwing_vector&
  operator=(throwing_vector&& other)
  {
    static_cast<std::vector<T>&>(*this) = std::move(other);
    if (throw_on_move)
      throw std::runtime_error("move assign");
    return *this;
  }
};

template<template<typename> class KC, template<typename> class MC>
void
test11()
{
#if __cpp_exceptions
  using flat_map = std::flat_map<int, int, std::less<int>, KC<int>, MC<int>>;

  auto is_really_empty = [](const flat_map& m) {
    return m.empty() && m.keys().empty() && m.values().empty();
  };
  throwing_vector<int>::throw_on_move = true;

  // Verify invariant preservation upon throwing move construction.
  flat_map source;
  source.insert({{1, 100}, {2, 200}});
  try
    {
      flat_map target(std::move(source));
      VERIFY( false );
    }
  catch (const std::runtime_error&)
    {
      VERIFY( is_really_empty(source) );
    }

  // Verify invariant preservation upon throwing move assignment.
  source = {{1, 100}, {2, 200}};
  flat_map target;
  target.insert({{3, 300}, {4, 400}});
  try
    {
      target = std::move(source);
      VERIFY( false );
    }
  catch (const std::runtime_error&)
    {
      VERIFY( is_really_empty(source) );
      VERIFY( is_really_empty(target) );
    }

  // Verify invariant preservation upon throwing swap.
  source = {{1, 100}, {2, 200}};
  target = {{3, 300}, {4, 400}};
  try
    {
      source.swap(target);
      VERIFY( false );
    }
  catch (const std::runtime_error&)
    {
      VERIFY( is_really_empty(source) );
      VERIFY( is_really_empty(target) );
    }
#endif
}

constexpr
void
test12()
{
  // Verify usability of flat_map::insert_range(sorted_unique_t, Rg&&).
  std::flat_map<int, int> m = {{2, 200}};
  std::pair<int, int> s[] = {{1, 100}, {3, 300}};
  m.insert_range(std::sorted_unique, s);
  VERIFY( std::ranges::equal(m.keys(), (int[]){1, 2, 3}) );
  VERIFY( std::ranges::equal(m.values(), (int[]){100, 200, 300}) );
}

void
test13()
{
  // Verify usability of flat_map::operator=(initializer_list).
  throwing_vector<int>::throw_on_move = true;
  std::flat_map<int, int, std::less<int>, throwing_vector<int>> s;
  std::initializer_list<std::pair<int, int>> il = {{2, 1}, {3, 2}, {1, 3}};
  s = il;
  VERIFY( std::ranges::equal(s.keys(), (int[]){1, 2, 3}) );
  VERIFY( std::ranges::equal(s.values(), (int[]){3, 1, 2}) );
}

void
test14()
{
  // Verify optimal number of moves in flat_map::insert_range for sorted_unique
  static int moves;
  struct counter
  {
    int val;
    constexpr counter() = default;
    constexpr counter(int v) : val(v) {}
    constexpr counter(const counter&) = default;
    constexpr counter(counter&& o) noexcept : val(o.val) { ++moves; }
    constexpr counter& operator=(const counter& o) = default;
    constexpr counter& operator=(counter&& o) noexcept {
      val = o.val;
      ++moves;
      return *this;
    }
    constexpr bool operator==(const counter&) const = default;
    constexpr auto operator<=>(const counter& o) const = default;
  };

  std::flat_map<counter, counter> m;
  std::pair<counter, counter> r[] = {
    {counter(1), counter(10)},
    {counter(2), counter(20)},
    {counter(3), counter(30)},
  };

  auto [keys,values] = std::move(m).extract();
  keys.reserve(std::size(r));
  values.reserve(std::size(r));
  m.replace(std::move(keys), std::move(values));

  moves = 0;
  m.insert_range(std::sorted_unique, std::views::as_rvalue(r));
  VERIFY( moves == 6 );
}

void
test()
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
  test08();
  test09();
  test10();
  test11<std::vector, throwing_vector>();
  test11<throwing_vector, std::vector>();
  test12();
  test13();
  test14();
}

constexpr
bool
test_constexpr()
{
  test01<std::vector, std::vector>();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
#if __cpp_lib_constexpr_string >= 201907L
  test10();
#endif
  // test11() is non-constexpr
  test12();
  // test13() is non-constexpr
  // test14() is non-constexpr
  return true;
}

int
main()
{
  test();
#if __cplusplus > 202302L
  static_assert(test_constexpr());
#if __cpp_lib_constexpr_flat_map != 202502L
#error "Feature-test macro __cpp_lib_constexpr_flat_map has wrong value in <flat_map>"
#endif
#endif
}
