// Verify P2165R4 enhancements to std::pair.
// { dg-do run { target c++23 } }

#include <array>
#include <tuple>
#include <utility>
#include <testsuite_hooks.h>

using std::array;
using std::pair;
using std::tuple;

struct A { };

template<template<typename> class pair_like_t>
constexpr bool
test01()
{
  struct B {
    int m;
    constexpr B(A&) : m(0) { }
    constexpr B(A&&) : m(1) { }
    constexpr B(const A&) : m(2) { }
    constexpr B(const A&&) : m(3) { }
  };

  // template<pair-like UPair>
  //   constexpr explicit(false) pair(UPair&&);

  pair_like_t<A> pair_like;

  [&] {
    pair<B, B> p2b = pair_like;
    VERIFY( p2b.first.m == 0 && p2b.second.m == 0 );
  }();
  [&] {
    pair<B, B> p2b = std::move(pair_like);
    VERIFY( p2b.first.m == 1 && p2b.second.m == 1 );
  }();
  [&] {
    pair<B, B> p2b = std::as_const(pair_like);
    VERIFY( p2b.first.m == 2 && p2b.second.m == 2 );
  }();
  [&] {
    pair<B, B> p2b = std::move(std::as_const(pair_like));
    VERIFY( p2b.first.m == 3 && p2b.second.m == 3 );
  }();

  // Verify dangling checks.
  static_assert( !std::is_constructible_v<pair<const int&, int>, pair_like_t<long>> );
  static_assert( !std::is_constructible_v<pair<int, const int&>, pair_like_t<long>> );

  return true;
}

template<template<typename> class pair_like_t>
constexpr bool
test02()
{
  struct B {
    int m;
    constexpr explicit B(A&) : m(0) { }
    constexpr explicit B(A&&) : m(1) { }
    constexpr explicit B(const A&) : m(2) { }
    constexpr explicit B(const A&&) : m(3) { }
  };

  // template<pair-like UPair>
  //   constexpr explicit(true) pair(UPair&&);

  static_assert( !std::is_convertible_v<pair_like_t<A>, pair<B, B>> );

  pair_like_t<A> pair_like;

  [&] {
    pair<B, B> p2b{pair_like};
    VERIFY( p2b.first.m == 0 && p2b.second.m == 0 );
  }();
  [&] {
    pair<B, B> p2b{std::move(pair_like)};
    VERIFY( p2b.first.m == 1 && p2b.second.m == 1 );
  }();
  [&] {
    pair<B, B> p2b{std::as_const(pair_like)};
    VERIFY( p2b.first.m == 2 && p2b.second.m == 2 );
  }();
  [&] {
    pair<B, B> p2b{std::move(std::as_const(pair_like))};
    VERIFY( p2b.first.m == 3 && p2b.second.m == 3 );
  }();

  return true;
}

template<template<typename> class pair_like_t>
constexpr bool
test03()
{
  struct B {
    int m;
    constexpr B& operator=(A&) { m = 0; return *this; }
    constexpr B& operator=(A&&) { m = 1; return *this; }
    constexpr B& operator=(const A&) { m = 2; return *this; }
    constexpr B& operator=(const A&&) { m = 3; return *this; }
  };

  // template<pair-like UPair>
  //   constexpr pair& operator=(UPair&&);

  pair_like_t<A> pair_like;

  pair<B, B> p2b;
  p2b = pair_like;
  VERIFY( p2b.first.m == 0 && p2b.second.m == 0 );
  p2b = std::move(pair_like);
  VERIFY( p2b.first.m == 1 && p2b.second.m == 1 );
  p2b = std::as_const(pair_like);
  VERIFY( p2b.first.m == 2 && p2b.second.m == 2 );
  p2b = std::move(std::as_const(pair_like));
  VERIFY( p2b.first.m == 3 && p2b.second.m == 3 );

  return true;
}

template<template<typename> class pair_like_t>
constexpr bool
test04()
{
  struct B {
    mutable int m;
    constexpr const B& operator=(A&) const { m = 0; return *this; }
    constexpr const B& operator=(A&&) const { m = 1; return *this; }
    constexpr const B& operator=(const A&) const { m = 2; return *this; }
    constexpr const B& operator=(const A&&) const { m = 3; return *this; }
  };

  // template<pair-like UPair>
  //   constexpr const pair& operator=(UPair&&) const;

  pair_like_t<A> pair_like;

  const pair<B, B> p2b;
  p2b = pair_like;
  VERIFY( p2b.first.m == 0 && p2b.second.m == 0 );
  p2b = std::move(pair_like);
  VERIFY( p2b.first.m == 1 && p2b.second.m == 1 );
  p2b = std::as_const(pair_like);
  VERIFY( p2b.first.m == 2 && p2b.second.m == 2 );
  p2b = std::move(std::as_const(pair_like));
  VERIFY( p2b.first.m == 3 && p2b.second.m == 3 );

  return true;
}

template<typename T>
using pair_like_array = array<T, 2>;

template<typename T>
using pair_like_tuple = tuple<T, T>;

int
main()
{
  static_assert( test01<pair_like_array>() );
  static_assert( test02<pair_like_array>() );
  static_assert( test03<pair_like_array>() );
  static_assert( test04<pair_like_array>() );

  static_assert( test01<pair_like_tuple>() );
  static_assert( test02<pair_like_tuple>() );
  static_assert( test03<pair_like_tuple>() );
  static_assert( test04<pair_like_tuple>() );
}
