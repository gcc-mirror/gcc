// Verify P2165R4 enhancements to std::tuple.
// { dg-do run { target c++23 } }

#include <array>
#include <tuple>
#include <utility>
#include <memory>
#include <testsuite_hooks.h>

using std::array;
using std::pair;
using std::tuple;
using std::allocator;
using std::allocator_arg_t;
using std::allocator_arg;

namespace alloc {
  struct B01;
  struct B02;
}

template<> struct std::uses_allocator<alloc::B01, allocator<int>> : std::true_type { };
template<> struct std::uses_allocator<alloc::B02, allocator<int>> : std::true_type { };

struct A { };

template<template<typename> class tuple_like_t>
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

  // template<tuple-like UTuple>
  //   constexpr explicit(false) tuple(UTuple&&);

  tuple_like_t<A> tuple_like;

  [&] {
    tuple<B, B, B> t3b = tuple_like;
    VERIFY( std::get<0>(t3b).m == 0 && std::get<1>(t3b).m == 0 && std::get<2>(t3b).m == 0 );
  }();
  [&] {
    tuple<B, B, B> t3b = std::move(tuple_like);
    VERIFY( std::get<0>(t3b).m == 1 && std::get<1>(t3b).m == 1 && std::get<2>(t3b).m == 1 );
  }();
  [&] {
    tuple<B, B, B> t3b = std::as_const(tuple_like);
    VERIFY( std::get<0>(t3b).m == 2 && std::get<1>(t3b).m == 2 && std::get<2>(t3b).m == 2 );
  }();
  [&] {
    tuple<B, B, B> t3b = std::move(std::as_const(tuple_like));
    VERIFY( std::get<0>(t3b).m == 3 && std::get<1>(t3b).m == 3 && std::get<2>(t3b).m == 3 );
  }();

  // Verify dangling checks.
  static_assert( !std::is_constructible_v<tuple<const int&, int, int>, tuple_like_t<long>> );
  static_assert( !std::is_constructible_v<tuple<int, const int&, int>, tuple_like_t<long>> );
  static_assert( !std::is_constructible_v<tuple<int, int, const int&>, tuple_like_t<long>> );

  return true;
}

namespace alloc
{
  struct B01 {
    int m;
    B01(A&);
    B01(A&&);
    B01(const A&);
    B01(const A&&);
    constexpr B01(allocator_arg_t, allocator<int>, A&) : m(0) { }
    constexpr B01(allocator_arg_t, allocator<int>, A&&) : m(1) { }
    constexpr B01(allocator_arg_t, allocator<int>, const A&) : m(2) { }
    constexpr B01(allocator_arg_t, allocator<int>, const A&&) : m(3) { }
  };

  template<template<typename> class tuple_like_t>
  constexpr bool
  test01()
  {
    using B = B01;

    // template<tuple-like UTuple>
    //   constexpr explicit(false) tuple(allocator_arg_t, const Alloc&, UTuple&&);

    tuple_like_t<A> tuple_like;

    [&] {
      tuple<B, B, B> t3b = {allocator_arg, allocator<int>{}, tuple_like};
      VERIFY( std::get<0>(t3b).m == 0 && std::get<1>(t3b).m == 0 && std::get<2>(t3b).m == 0 );
    }();
    [&] {
      tuple<B, B, B> t3b = {allocator_arg, allocator<int>{}, std::move(tuple_like)};
      VERIFY( std::get<0>(t3b).m == 1 && std::get<1>(t3b).m == 1 && std::get<2>(t3b).m == 1 );
    }();
    [&] {
      tuple<B, B, B> t3b = {allocator_arg, allocator<int>{}, std::as_const(tuple_like)};
      VERIFY( std::get<0>(t3b).m == 2 && std::get<1>(t3b).m == 2 && std::get<2>(t3b).m == 2 );
    }();
    [&] {
      tuple<B, B, B> t3b = {allocator_arg, allocator<int>{}, std::move(std::as_const(tuple_like))};
      VERIFY( std::get<0>(t3b).m == 3 && std::get<1>(t3b).m == 3 && std::get<2>(t3b).m == 3 );
    }();

  // Verify dangling checks.
    static_assert( !std::is_constructible_v<tuple<const int&, int, int>,
					    allocator_arg_t, allocator<int>,
					    tuple_like_t<long>> );
    static_assert( !std::is_constructible_v<tuple<int, const int&, int>,
					    allocator_arg_t, allocator<int>,
					    tuple_like_t<long>> );
    static_assert( !std::is_constructible_v<tuple<int, int, const int&>,
					    allocator_arg_t, allocator<int>,
					    tuple_like_t<long>> );

    return true;
  }
}

template<template<typename> class tuple_like_t>
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

  // template<tuple-like UTuple>
  //   constexpr explicit(true) tuple(UTuple&&);

  static_assert( !std::is_convertible_v<tuple_like_t<A>, tuple<B, B, B>> );

  tuple_like_t<A> tuple_like;

  [&] {
    tuple<B, B, B> t3b{tuple_like};
    VERIFY( std::get<0>(t3b).m == 0 && std::get<1>(t3b).m == 0 && std::get<2>(t3b).m == 0 );
  }();
  [&] {
    tuple<B, B, B> t3b{std::move(tuple_like)};
    VERIFY( std::get<0>(t3b).m == 1 && std::get<1>(t3b).m == 1 && std::get<2>(t3b).m == 1 );
  }();
  [&] {
    tuple<B, B, B> t3b{std::as_const(tuple_like)};
    VERIFY( std::get<0>(t3b).m == 2 && std::get<1>(t3b).m == 2 && std::get<2>(t3b).m == 2 );
  }();
  [&] {
    tuple<B, B, B> t3b{std::move(std::as_const(tuple_like))};
    VERIFY( std::get<0>(t3b).m == 3 && std::get<1>(t3b).m == 3 && std::get<2>(t3b).m == 3 );
  }();

  return true;
}

namespace alloc
{
  struct B02 {
    int m;
    explicit B02(A&);
    explicit B02(A&&);
    explicit B02(const A&);
    explicit B02(const A&&);
    explicit constexpr B02(allocator_arg_t, allocator<int>, A&) : m(0) { }
    explicit constexpr B02(allocator_arg_t, allocator<int>, A&&) : m(1) { }
    explicit constexpr B02(allocator_arg_t, allocator<int>, const A&) : m(2) { }
    explicit constexpr B02(allocator_arg_t, allocator<int>, const A&&) : m(3) { }
  };

  template<template<typename> class tuple_like_t>
  constexpr bool
  test02()
  {
    using B = B02;

    // template<tuple-like UTuple>
    //   constexpr explicit(true) tuple(allocator_arg_t, const Alloc&, UTuple&&);

    static_assert( !std::is_convertible_v<tuple_like_t<A>, tuple<B, B, B>> );

    tuple_like_t<A> tuple_like;

    [&] {
      tuple<B, B, B> t3b{allocator_arg, allocator<int>{}, tuple_like};
      VERIFY( std::get<0>(t3b).m == 0 && std::get<1>(t3b).m == 0 && std::get<2>(t3b).m == 0 );
    }();
    [&] {
      tuple<B, B, B> t3b{allocator_arg, allocator<int>{}, std::move(tuple_like)};
      VERIFY( std::get<0>(t3b).m == 1 && std::get<1>(t3b).m == 1 && std::get<2>(t3b).m == 1 );
    }();
    [&] {
      tuple<B, B, B> t3b{allocator_arg, allocator<int>{}, std::as_const(tuple_like)};
      VERIFY( std::get<0>(t3b).m == 2 && std::get<1>(t3b).m == 2 && std::get<2>(t3b).m == 2 );
    }();
    [&] {
      tuple<B, B, B> t3b{allocator_arg, allocator<int>{}, std::move(std::as_const(tuple_like))};
      VERIFY( std::get<0>(t3b).m == 3 && std::get<1>(t3b).m == 3 && std::get<2>(t3b).m == 3 );
    }();

    return true;
  }
}


template<template<typename> class tuple_like_t>
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

  // template<tuple-like UTuple>
  //   constexpr tuple& operator=(UTuple&&);

  tuple_like_t<A> tuple_like;

  tuple<B, B, B> t3b;
  t3b = tuple_like;
  VERIFY( std::get<0>(t3b).m == 0 && std::get<1>(t3b).m == 0 && std::get<2>(t3b).m == 0 );
  t3b = std::move(tuple_like);
  VERIFY( std::get<0>(t3b).m == 1 && std::get<1>(t3b).m == 1 && std::get<2>(t3b).m == 1 );
  t3b = std::as_const(tuple_like);
  VERIFY( std::get<0>(t3b).m == 2 && std::get<1>(t3b).m == 2 && std::get<2>(t3b).m == 2 );
  t3b = std::move(std::as_const(tuple_like));
  VERIFY( std::get<0>(t3b).m == 3 && std::get<1>(t3b).m == 3 && std::get<2>(t3b).m == 3 );

  return true;
}

template<template<typename> class tuple_like_t>
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

  // template<tuple-like UTuple>
  //   constexpr const tuple& operator=(UTuple&&) const;

  tuple_like_t<A> tuple_like;

  const tuple<B, B, B> t3b;
  t3b = tuple_like;
  VERIFY( std::get<0>(t3b).m == 0 && std::get<1>(t3b).m == 0 && std::get<2>(t3b).m == 0 );
  t3b = std::move(tuple_like);
  VERIFY( std::get<0>(t3b).m == 1 && std::get<1>(t3b).m == 1 && std::get<2>(t3b).m == 1 );
  t3b = std::as_const(tuple_like);
  VERIFY( std::get<0>(t3b).m == 2 && std::get<1>(t3b).m == 2 && std::get<2>(t3b).m == 2 );
  t3b = std::move(std::as_const(tuple_like));
  VERIFY( std::get<0>(t3b).m == 3 && std::get<1>(t3b).m == 3 && std::get<2>(t3b).m == 3 );

  return true;
}

template<template<typename> class tuple_like_t>
constexpr bool
test05()
{
  // template<tuple-like UTuple>
  //   constexpr bool operator==(const tuple&, const UTuple&);

  static_assert( tuple{1, 2, 3} == tuple_like_t{1, 2, 3} );
  static_assert( tuple{1, 2, 4} != tuple_like_t{1, 2, 3} );
  static_assert( tuple_like_t{1, 2, 3} == tuple{1, 2, 3} );
  static_assert( tuple_like_t{1, 2, 3} != tuple{1, 2, 4} );

  // template<tuple-like UTuple>
  //   constexpr bool operator<=>const tuple&, const UTuple&);

  static_assert( (tuple{1, 2, 3} <=> tuple_like_t{1, 2, 3}) == std::strong_ordering::equal );
  static_assert( (tuple{1, 2, 4} <=> tuple_like_t{1, 2, 3}) == std::strong_ordering::greater );
  static_assert( (tuple_like_t{1, 2, 3} <=> tuple{1, 2, 3}) == std::strong_ordering::equal );
  static_assert( (tuple_like_t{1, 2, 3} <=> tuple{1, 2, 4}) == std::strong_ordering::less  );

  static_assert( tuple{1, 2, 4} > tuple_like_t{1, 2, 3} );
  static_assert( tuple_like_t{1, 2, 3} < tuple{1, 2, 4} );

  // template<tuple-like TTuple, tuple-like UTuple, ...>
  //   struct basic_common_reference<TTuple, UTuple, ...>;

  static_assert( std::same_as<std::common_reference_t<tuple_like_t<int>,
						      tuple<int, long, int>>,
			      tuple<int, long, int>> );

  static_assert( std::same_as<std::common_reference_t<tuple<int, long, int>,
						      tuple_like_t<int>>,
			      tuple<int, long, int>> );

  // template<tuple-like TTuple, tuple-like UTuple>
  //   struct common_type<TTuple, UTuple>;

  static_assert( std::same_as<std::common_type_t<tuple_like_t<const int&>,
						 tuple<int, long, int>>,
			      tuple<int, long, int>> );

  static_assert( std::same_as<std::common_type_t<tuple<int, long, int>,
						 tuple_like_t<const int&>>,
			      tuple<int, long, int>> );

  return true;
}

template<typename T>
using tuple_like_array = array<T, 3>;

int
main()
{
  static_assert( test01<tuple_like_array>() );
  static_assert( alloc::test01<tuple_like_array>() );
  static_assert( test02<tuple_like_array>() );
  static_assert( alloc::test02<tuple_like_array>() );
  static_assert( test03<tuple_like_array>() );
  static_assert( test04<tuple_like_array>() );
  static_assert( test05<tuple_like_array>() );
}
