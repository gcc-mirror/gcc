// Verify P2321R2 "zip" enhancements to std::pair.
// { dg-do run { target c++23 } }

#include <utility>
#include <testsuite_hooks.h>

using std::pair;

struct A { };

constexpr bool
test01()
{
  struct B { bool v; constexpr B(A&) : v(true) { } };

  // template<class U1, class U2>
  //   constexpr explicit(false) pair(pair<U1, U2>&);

  pair<A, int> p2a0;
  pair<B, int> p2b0 = p2a0;
  VERIFY( std::get<0>(p2b0).v );

  pair<int, A> p2a1;
  pair<int, B> p2b1 = p2a1;
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

constexpr bool
test02()
{
  struct B { bool v; explicit constexpr B(A&) : v(true) { } };

  // template<class U1, class U2>
  //   constexpr explicit(true) pair(pair<U1, U2>&);

  static_assert(!std::is_convertible_v<pair<A, int>&, pair<B, int>>);
  static_assert(!std::is_convertible_v<pair<int, A>&, pair<int, B>>);

  pair<A, int> p2a0;
  pair<B, int> p2b0(p2a0);
  VERIFY( std::get<0>(p2b0).v );

  pair<int, A> p2a1;
  pair<int, B> p2b1(p2a1);
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

constexpr bool
test03()
{
  struct B { bool v; constexpr B(const A&&) : v(true) { } };

  // template<class U1, class U2>
  //   constexpr explicit(false) pair(const pair<U1, U2>&&);

  const pair<A, int> p2a0;
  pair<B, int> p2b0 = std::move(p2a0);
  VERIFY( std::get<0>(p2b0).v );

  const pair<int, A> p2a1;
  pair<int, B> p2b1 = std::move(p2a1);
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

constexpr bool
test04()
{
  struct B { bool v; explicit constexpr B(const A&&) : v(true) { } };

  // template<class U1, class U2>
  //   constexpr explicit(true) pair(const pair<U1, U2>&&);

  static_assert(!std::is_convertible_v<const pair<A, int>&&, pair<B, int>>);
  static_assert(!std::is_convertible_v<const pair<int, A>&&, pair<int, B>>);

  const pair<A, int> p2a0;
  pair<B, int> p2b0(std::move(p2a0));
  VERIFY( std::get<0>(p2b0).v );

  const pair<int, A> p2a1;
  pair<int, B> p2b1(std::move(p2a1));
  VERIFY( std::get<1>(p2b1).v );

  return true;
}

constexpr bool
test05()
{
  struct B
  {
    mutable bool v;
    constexpr const B& operator=(const A&) const { v = true; return *this; }
  };

  // template<class U1, class U2>
  //   constexpr const pair& operator=(const pair<U1, U2>&) const;

  const pair<A, A> p2a;
  const pair<B, B> p2b;
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

  // template<class U1, class U2>
  //   constexpr const pair& operator=(pair<U1, U2>&&) const;

  pair<A, A> p2a;
  const pair<B, B> p2b;
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

  // constexpr const pair& operator=(const pair&) const;

  const pair<B, B> t2a;
  const pair<B, B> t2b;
  t2b = t2a;
  VERIFY( std::get<0>(t2b).v );
  VERIFY( std::get<1>(t2b).v );

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

  // constexpr const pair& operator=(pair&&) const;

  pair<B, B> t2a;
  const pair<B, B> t2b;
  t2b = std::move(t2a);
  VERIFY( std::get<0>(t2b).v );
  VERIFY( std::get<1>(t2b).v );

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
  const pair<S, S> t2, u2;
  std::swap(t2, u2);
  VERIFY( std::get<0>(t2).v == 1 );
  VERIFY( std::get<0>(u2).v == 1 );
  VERIFY( std::get<1>(t2).v == 1 );
  VERIFY( std::get<1>(u2).v == 1 );

  static_assert(!std::is_swappable_v<const pair<A, int>&>);
  static_assert(!std::is_swappable_v<const pair<int, A>&>);

  return true;
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
  static_assert(test04());
  // FIXME: G++ doesn't support reading mutable members during constexpr (PR c++/92505).
  test05();
  test06();
  test07();
  test08();
  test09();
}
