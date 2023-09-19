// Verify P2321R2 "zip" enhancements to std::tuple.
// { dg-do run { target c++23 } }
// FIXME [!HOSTED]: avoidable std::allocator usage
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <tuple>

#if __cpp_lib_ranges_zip != 202110L
# error "Feature-test macro __cpp_lib_ranges_zip has wrong value in <tuple>"
#endif

#include <memory>
#include <testsuite_hooks.h>

using std::tuple;
using std::pair;
using std::allocator;
using std::allocator_arg_t;
using std::allocator_arg;

namespace alloc {
  struct B01;
  struct B02;
  struct B03;
  struct B04;
}

template<> struct std::uses_allocator<alloc::B01, allocator<int>> : std::true_type { };
template<> struct std::uses_allocator<alloc::B02, allocator<int>> : std::true_type { };
template<> struct std::uses_allocator<alloc::B03, allocator<int>> : std::true_type { };
template<> struct std::uses_allocator<alloc::B04, allocator<int>> : std::true_type { };

struct A { };

constexpr bool
test01()
{
  struct B { bool v; constexpr B(A&) : v(true) { } };

  // template<class... UTypes>
  //   constexpr explicit(false) tuple(tuple<UTypes...>&);

  tuple<A> t1a;
  tuple<B> t1b = t1a;
  VERIFY( std::get<0>(t1b).v );

  tuple<A, int> t2a0;
  tuple<B, int> t2b0 = t2a0;
  VERIFY( std::get<0>(t2b0).v );

  tuple<int, A> t2a1;
  tuple<int, B> t2b1 = t2a1;
  VERIFY( std::get<1>(t2b1).v );

  tuple<A, int, int> t3a0;
  tuple<B, int, int> t3b0 = t3a0;
  VERIFY( std::get<0>(t3b0).v );

  tuple<int, A, int> t3a1;
  tuple<int, B, int> t3b1 = t3a1;
  VERIFY( std::get<1>(t3b1).v );

  tuple<int, int, A> t3a2;
  tuple<int, int, B> t3b2 = t3a2;
  VERIFY( std::get<2>(t3b2).v );

  // template<class... UTypes>
  //   constexpr explicit(false) tuple(pair<UTypes...>&);

  pair<A, int> p2a0;
  tuple<B, int> p2b0 = p2a0;
  VERIFY( std::get<0>(p2b0).v );

  pair<int, A> p2a1;
  tuple<int, B> p2b1 = p2a1;
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

namespace alloc
{
  struct B01
  {
    bool v;
    B01(A&);
    constexpr B01(allocator_arg_t, allocator<int>, A&) : v(true) { }
  };

  constexpr bool
  test01()
  {
    using B = B01;

    // template<class Alloc, class... UTypes>
    //   constexpr explicit(false)
    //     tuple(allocator_arg_t, const Alloc& a, tuple<UTypes...>&);

    tuple<A> t1a;
    tuple<B> t1b = {allocator_arg, allocator<int>{}, t1a};
    VERIFY( std::get<0>(t1b).v );

    tuple<A, int> t2a0;
    tuple<B, int> t2b0 = {allocator_arg, allocator<int>{}, t2a0};
    VERIFY( std::get<0>(t2b0).v );

    tuple<int, A> t2a1;
    tuple<int, B> t2b1 = {allocator_arg, allocator<int>{}, t2a1};
    VERIFY( std::get<1>(t2b1).v );

    tuple<A, int, int> t3a0;
    tuple<B, int, int> t3b0 = {allocator_arg, allocator<int>{}, t3a0};
    VERIFY( std::get<0>(t3b0).v );

    tuple<int, A, int> t3a1;
    tuple<int, B, int> t3b1 = {allocator_arg, allocator<int>{}, t3a1};
    VERIFY( std::get<1>(t3b1).v );

    tuple<int, int, A> t3a2;
    tuple<int, int, B> t3b2 = {allocator_arg, allocator<int>{}, t3a2};
    VERIFY( std::get<2>(t3b2).v );

    // template<class Alloc, class U1, class U2>
    //   constexpr explicit(false)
    //     tuple(allocator_arg_t, const Alloc& a, pair<U1, U2>&);

    pair<A, int> p2a0;
    tuple<B, int> p2b0 = {allocator_arg, allocator<int>{}, p2a0};
    VERIFY( std::get<0>(p2b0).v );

    pair<int, A> p2a1;
    tuple<int, B> p2b1 = {allocator_arg, allocator<int>{}, p2a1};
    VERIFY( std::get<1>(p2b1).v );

    return true;
  }
}

constexpr bool
test02()
{
  struct B { bool v; explicit constexpr B(A&) : v(true) { } };

  // template<class... UTypes>
  //   constexpr explicit(true) tuple(tuple<UTypes...>&);

  static_assert(!std::is_convertible_v<tuple<A>&, tuple<B>>);

  tuple<A> t1a;
  tuple<B> t1b(t1a);
  VERIFY( std::get<0>(t1b).v );

  static_assert(!std::is_convertible_v<tuple<A, int>&, tuple<B, int>>);
  static_assert(!std::is_convertible_v<tuple<int, A>&, tuple<int, B>>);

  tuple<A, int> t2a0;
  tuple<B, int> t2b0(t2a0);
  VERIFY( std::get<0>(t2b0).v );

  tuple<int, A> t2a1;
  tuple<int, B> t2b1(t2a1);
  VERIFY( std::get<1>(t2b1).v );

  static_assert(!std::is_convertible_v<tuple<A, int, int>&, tuple<B, int, int>>);
  static_assert(!std::is_convertible_v<tuple<int, A, int>&, tuple<int, B, int>>);
  static_assert(!std::is_convertible_v<tuple<int, int, A>&, tuple<int, int, B>>);

  tuple<A, int, int> t3a0;
  tuple<B, int, int> t3b0(t3a0);
  VERIFY( std::get<0>(t3b0).v );

  tuple<int, A, int> t3a1;
  tuple<int, B, int> t3b1(t3a1);
  VERIFY( std::get<1>(t3b1).v );

  tuple<int, int, A> t3a2;
  tuple<int, int, B> t3b2(t3a2);
  VERIFY( std::get<2>(t3b2).v );

  // template<class... UTypes>
  //   constexpr explicit(true) tuple(pair<UTypes...>&);

  static_assert(!std::is_convertible_v<pair<A, int>&, tuple<B, int>>);
  static_assert(!std::is_convertible_v<pair<int, A>&, tuple<int, B>>);

  pair<A, int> p2a0;
  tuple<B, int> p2b0(p2a0);
  VERIFY( std::get<0>(p2b0).v );

  pair<int, A> p2a1;
  tuple<int, B> p2b1(p2a1);
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

namespace alloc
{
  struct B02
  {
    bool v;
    explicit B02(A&);
    explicit constexpr B02(allocator_arg_t, allocator<int>, A&) : v(true) { }
  };

  constexpr bool
  test02()
  {
    using B = B02;

    // template<class Alloc, class... UTypes>
    //   constexpr explicit(true)
    //     tuple(allocator_arg_t, const Alloc& a, tuple<UTypes...>&);

    tuple<A> t1a;
    tuple<B> t1b(allocator_arg, allocator<int>{}, t1a);
    VERIFY( std::get<0>(t1b).v );

    tuple<A, int> t2a0;
    tuple<B, int> t2b0(allocator_arg, allocator<int>{}, t2a0);
    VERIFY( std::get<0>(t2b0).v );

    tuple<int, A> t2a1;
    tuple<int, B> t2b1(allocator_arg, allocator<int>{}, t2a1);
    VERIFY( std::get<1>(t2b1).v );

    tuple<A, int, int> t3a0;
    tuple<B, int, int> t3b0(allocator_arg, allocator<int>{}, t3a0);
    VERIFY( std::get<0>(t3b0).v );

    tuple<int, A, int> t3a1;
    tuple<int, B, int> t3b1(allocator_arg, allocator<int>{}, t3a1);
    VERIFY( std::get<1>(t3b1).v );

    tuple<int, int, A> t3a2;
    tuple<int, int, B> t3b2(allocator_arg, allocator<int>{}, t3a2);
    VERIFY( std::get<2>(t3b2).v );

    // template<class Alloc, class U1, class U2>
    //   constexpr explicit(true)
    //     tuple(allocator_arg_t, const Alloc& a, pair<U1, U2>&);

    pair<A, int> p2a0;
    tuple<B, int> p2b0(allocator_arg, allocator<int>{}, p2a0);
    VERIFY( std::get<0>(p2b0).v );

    pair<int, A> p2a1;
    tuple<int, B> p2b1(allocator_arg, allocator<int>{}, p2a1);
    VERIFY( std::get<1>(p2b1).v );

    return true;
  }
} // namespace alloc

constexpr bool
test03()
{
  struct B { bool v; constexpr B(const A&&) : v(true) { } };

  // template<class... UTypes>
  //   constexpr explicit(false) tuple(const tuple<UTypes...>&&);

  const tuple<A> t1a;
  tuple<B> t1b = std::move(t1a);
  VERIFY( std::get<0>(t1b).v );

  const tuple<A, int> t2a0;
  tuple<B, int> t2b0 = std::move(t2a0);
  VERIFY( std::get<0>(t2b0).v );

  const tuple<int, A> t2a1;
  tuple<int, B> t2b1 = std::move(t2a1);
  VERIFY( std::get<1>(t2b1).v );

  const tuple<A, int, int> t3a0;
  tuple<B, int, int> t3b0 = std::move(t3a0);
  VERIFY( std::get<0>(t3b0).v );

  const tuple<int, A, int> t3a1;
  tuple<int, B, int> t3b1 = std::move(t3a1);
  VERIFY( std::get<1>(t3b1).v );

  const tuple<int, int, A> t3a2;
  tuple<int, int, B> t3b2 = std::move(t3a2);
  VERIFY( std::get<2>(t3b2).v );

  // template<class... UTypes>
  //   constexpr explicit(false) tuple(const pair<UTypes...>&&);

  const pair<A, int> p2a0;
  tuple<B, int> p2b0 = std::move(p2a0);
  VERIFY( std::get<0>(p2b0).v );

  const pair<int, A> p2a1;
  tuple<int, B> p2b1 = std::move(p2a1);
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

namespace alloc
{
  struct B03
  {
    bool v;
    B03(const A&&);
    constexpr B03(allocator_arg_t, allocator<int>, const A&&) : v(true) { }
  };

  constexpr bool
  test03()
  {
    using B = B03;

    // template<class Alloc, class... UTypes>
    //   constexpr explicit(false)
    //     tuple(allocator_arg_t, const Alloc& a, const tuple<UTypes...>&&);

    const tuple<A> t1a;
    tuple<B> t1b = {allocator_arg, allocator<int>{}, std::move(t1a)};
    VERIFY( std::get<0>(t1b).v );

    const tuple<A, int> t2a0;
    tuple<B, int> t2b0 = {allocator_arg, allocator<int>{}, std::move(t2a0)};
    VERIFY( std::get<0>(t2b0).v );

    const tuple<int, A> t2a1;
    tuple<int, B> t2b1 = {allocator_arg, allocator<int>{}, std::move(t2a1)};
    VERIFY( std::get<1>(t2b1).v );

    const tuple<A, int, int> t3a0;
    tuple<B, int, int> t3b0 = {allocator_arg, allocator<int>{}, std::move(t3a0)};
    VERIFY( std::get<0>(t3b0).v );

    const tuple<int, A, int> t3a1;
    tuple<int, B, int> t3b1 = {allocator_arg, allocator<int>{}, std::move(t3a1)};
    VERIFY( std::get<1>(t3b1).v );

    const tuple<int, int, A> t3a2;
    tuple<int, int, B> t3b2 = {allocator_arg, allocator<int>{}, std::move(t3a2)};
    VERIFY( std::get<2>(t3b2).v );

    // template<class Alloc, class U1, class U2>
    //   constexpr explicit(false)
    //     tuple(allocator_arg_t, const Alloc& a, const pair<U1, U2>&&);

    const pair<A, int> p2a0;
    tuple<B, int> p2b0 = {allocator_arg, allocator<int>{}, std::move(p2a0)};
    VERIFY( std::get<0>(p2b0).v );

    const pair<int, A> p2a1;
    tuple<int, B> p2b1 = {allocator_arg, allocator<int>{}, std::move(p2a1)};
    VERIFY( std::get<1>(p2b1).v );

    return true;
  }
};

constexpr bool
test04()
{
  struct B { bool v; explicit constexpr B(const A&&) : v(true) { } };

  // template<class... UTypes>
  //   constexpr explicit(true) tuple(const tuple<UTypes...>&&);

  static_assert(!std::is_convertible_v<tuple<A>&, tuple<B>>);

  const tuple<A> t1a;
  tuple<B> t1b(std::move(t1a));
  VERIFY( std::get<0>(t1b).v );

  static_assert(!std::is_convertible_v<tuple<A, int>&, tuple<B, int>>);
  static_assert(!std::is_convertible_v<tuple<int, A>&, tuple<int, B>>);

  const tuple<A, int> t2a0;
  tuple<B, int> t2b0(std::move(t2a0));
  VERIFY( std::get<0>(t2b0).v );

  const tuple<int, A> t2a1;
  tuple<int, B> t2b1(std::move(t2a1));
  VERIFY( std::get<1>(t2b1).v );

  static_assert(!std::is_convertible_v<tuple<A, int, int>&, tuple<B, int, int>>);
  static_assert(!std::is_convertible_v<tuple<int, A, int>&, tuple<int, B, int>>);
  static_assert(!std::is_convertible_v<tuple<int, int, A>&, tuple<int, int, B>>);

  const tuple<A, int, int> t3a0;
  tuple<B, int, int> t3b0(std::move(t3a0));
  VERIFY( std::get<0>(t3b0).v );

  const tuple<int, A, int> t3a1;
  tuple<int, B, int> t3b1(std::move(t3a1));
  VERIFY( std::get<1>(t3b1).v );

  const tuple<int, int, A> t3a2;
  tuple<int, int, B> t3b2(std::move(t3a2));
  VERIFY( std::get<2>(t3b2).v );

  // template<class... UTypes>
  //   constexpr explicit(true) tuple(const pair<UTypes...>&&);

  static_assert(!std::is_convertible_v<pair<A, int>&, tuple<B, int>>);
  static_assert(!std::is_convertible_v<pair<int, A>&, tuple<int, B>>);

  const pair<A, int> p2a0;
  tuple<B, int> p2b0(std::move(p2a0));
  VERIFY( std::get<0>(p2b0).v );

  const pair<int, A> p2a1;
  tuple<int, B> p2b1(std::move(p2a1));
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

namespace alloc
{
  struct B04
  {
    bool v;
    explicit B04(const A&&);
    explicit constexpr B04(allocator_arg_t, allocator<int>, const A&&) : v(true) { }
  };

  constexpr bool
  test04()
  {
    using B = B04;

    // template<class Alloc, class... UTypes>
    //   constexpr explicit(true)
    //     tuple(allocator_arg_t, const Alloc& a, const tuple<UTypes...>&&);

    const tuple<A> t1a;
    tuple<B> t1b(allocator_arg, allocator<int>{}, std::move(t1a));
    VERIFY( std::get<0>(t1b).v );

    const tuple<A, int> t2a0;
    tuple<B, int> t2b0(allocator_arg, allocator<int>{}, std::move(t2a0));
    VERIFY( std::get<0>(t2b0).v );

    const tuple<int, A> t2a1;
    tuple<int, B> t2b1(allocator_arg, allocator<int>{}, std::move(t2a1));
    VERIFY( std::get<1>(t2b1).v );

    const tuple<A, int, int> t3a0;
    tuple<B, int, int> t3b0(allocator_arg, allocator<int>{}, std::move(t3a0));
    VERIFY( std::get<0>(t3b0).v );

    const tuple<int, A, int> t3a1;
    tuple<int, B, int> t3b1(allocator_arg, allocator<int>{}, std::move(t3a1));
    VERIFY( std::get<1>(t3b1).v );

    const tuple<int, int, A> t3a2;
    tuple<int, int, B> t3b2(allocator_arg, allocator<int>{}, std::move(t3a2));
    VERIFY( std::get<2>(t3b2).v );

    // template<class Alloc, class U1, class U2>
    //   constexpr explicit(true)
    //     tuple(allocator_arg_t, const Alloc& a, const pair<U1, U2>&&);

    tuple<B, int> p2b0(allocator_arg, allocator<int>{}, std::move(t2a0));
    VERIFY( std::get<0>(p2b0).v );

    tuple<int, B> p2b1(allocator_arg, allocator<int>{}, std::move(t2a1));
    VERIFY( std::get<1>(p2b1).v );

    return true;
  }
};

constexpr bool
test05()
{
  struct B
  {
    mutable bool v;
    constexpr const B& operator=(const A&) const { v = true; return *this; }
  };

  // template<class... UTypes>
  //   constexpr const tuple& operator=(const tuple<UTypes...>&) const;

  const tuple<A> t1a;
  const tuple<B> t1b;
  t1b = t1a;
  VERIFY( std::get<0>(t1b).v );

  const tuple<A, A> t2a;
  const tuple<B, B> t2b;
  t2b = t2a;
  VERIFY( std::get<0>(t2b).v );
  VERIFY( std::get<1>(t2b).v );

  const tuple<A, A, A> t3a;
  const tuple<B, B, B> t3b;
  t3b = t3a;
  VERIFY( std::get<0>(t3b).v );
  VERIFY( std::get<1>(t3b).v );
  VERIFY( std::get<2>(t3b).v );

  // template<class U1, class U2>
  //   constexpr const tuple& operator=(const pair<U1, U2>&) const;

  const pair<A, A> p2a;
  const tuple<B, B> p2b;
  p2b = p2a;

  return true;
}

constexpr bool
test06()
{
  struct B
  {
    mutable bool v;
    constexpr const B& operator=(A&&) const { v = true; return *this; }
  };

  // template<class... UTypes>
  //   constexpr const tuple& operator=(tuple<UTypes...>&&) const;

  tuple<A> t1a;
  const tuple<B> t1b;
  t1b = std::move(t1a);
  VERIFY( std::get<0>(t1b).v );

  tuple<A, A> t2a;
  const tuple<B, B> t2b;
  t2b = std::move(t2a);
  VERIFY( std::get<0>(t2b).v );
  VERIFY( std::get<1>(t2b).v );

  tuple<A, A, A> t3a;
  const tuple<B, B, B> t3b;
  t3b = std::move(t3a);
  VERIFY( std::get<0>(t3b).v );
  VERIFY( std::get<1>(t3b).v );
  VERIFY( std::get<2>(t3b).v );

  // template<class U1, class U2>
  //   constexpr const tuple& operator=(pair<U1, U2>&&) const;

  pair<A, A> p2a;
  const tuple<B, B> p2b;
  p2b = std::move(p2a);

  return true;
}

constexpr bool
test07()
{
  struct B
  {
    mutable bool v;
    constexpr const B& operator=(const B&) const { v = true; return *this; }
  };

  // constexpr const tuple& operator=(const tuple&) const;

  const tuple<B> t1a;
  const tuple<B> t1b;
  t1b = t1a;
  VERIFY( std::get<0>(t1b).v );

  const tuple<B, B> t2a;
  const tuple<B, B> t2b;
  t2b = t2a;
  VERIFY( std::get<0>(t2b).v );
  VERIFY( std::get<1>(t2b).v );

  const tuple<B, B, B> t3a;
  const tuple<B, B, B> t3b;
  t3b = t3a;
  VERIFY( std::get<0>(t3b).v );
  VERIFY( std::get<1>(t3b).v );
  VERIFY( std::get<2>(t3b).v );

  return true;
}

constexpr bool
test08()
{
  struct B
  {
    mutable bool v;
    constexpr const B& operator=(B&&) const { v = true; return *this; }
  };

  // constexpr const tuple& operator=(tuple&&) const;

  tuple<B> t1a;
  const tuple<B> t1b;
  t1b = std::move(t1a);
  VERIFY( std::get<0>(t1b).v );

  tuple<B, B> t2a;
  const tuple<B, B> t2b;
  t2b = std::move(t2a);
  VERIFY( std::get<0>(t2b).v );
  VERIFY( std::get<1>(t2b).v );

  tuple<B, B, B> t3a;
  const tuple<B, B, B> t3b;
  t3b = std::move(t3a);
  VERIFY( std::get<0>(t3b).v );
  VERIFY( std::get<1>(t3b).v );
  VERIFY( std::get<2>(t3b).v );

  return true;
}

struct S
{
  mutable int v = 0;
  friend constexpr void swap(S&& x, S&& y) = delete;
  friend constexpr void swap(const S& x, const S& y) { ++x.v; ++y.v; }
};

constexpr bool
test09()
{
  const tuple<S> t1, u1;
  std::swap(t1, u1);
  VERIFY( std::get<0>(t1).v == 1 );
  VERIFY( std::get<0>(u1).v == 1 );

  const tuple<S, S> t2, u2;
  std::swap(t2, u2);
  VERIFY( std::get<0>(t2).v == 1 );
  VERIFY( std::get<0>(u2).v == 1 );
  VERIFY( std::get<1>(t2).v == 1 );
  VERIFY( std::get<1>(u2).v == 1 );

  const tuple<S, S, S> t3, u3;
  std::swap(t3, u3);
  VERIFY( std::get<0>(t3).v == 1 );
  VERIFY( std::get<0>(u3).v == 1 );
  VERIFY( std::get<1>(t3).v == 1 );
  VERIFY( std::get<1>(u3).v == 1 );
  VERIFY( std::get<2>(t3).v == 1 );
  VERIFY( std::get<2>(u3).v == 1 );

  static_assert(!std::is_swappable_v<const tuple<A>&>);

  return true;
}

int
main()
{
  static_assert(test01());
  static_assert(alloc::test01());
  static_assert(test02());
  static_assert(alloc::test02());
  static_assert(test03());
  static_assert(alloc::test03());
  static_assert(test04());
  static_assert(alloc::test04());
  // FIXME: G++ doesn't support reading mutable members during constexpr (PR c++/92505).
  test05();
  test06();
  test07();
  test08();
  test09();
}
