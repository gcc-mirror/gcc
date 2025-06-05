// { dg-do run { target c++26 } }

#include <inplace_vector>
#include <ranges>
#include <testsuite_hooks.h>

struct X
{
  X() = default;
  constexpr X(int p) : v(p) {}
  constexpr X(const X& o) : v(o.v) { } // not trivial
  constexpr X(X&& o) : v(o.v) { } // not trivial

  constexpr X& operator=(const X& o) // not trivial
  { v = o.v; return *this; }
  constexpr X& operator=(X&& o) // not trivial
  { v = o.v; return *this; }

  int v;

  friend auto operator<=>(const X&, const X&) = default;
};

template<bool CNoex, bool ANoex>
struct N
{
  N() = default;
  constexpr N(N&&) noexcept(CNoex) { } // not trivial
  constexpr N& operator=(N&& o) noexcept(ANoex) // not trivial
  { return *this; }
};

struct D
{
  D() = default;
  D(D&&) = default;
  D& operator=(D&&) = default;
  ~D() {} // not trivially destructible
};

struct U
{
  U() = default;
  U(U&&) noexcept(false) = default; // lies about noexcept, is trivial but throwing
  U& operator=(U&&) noexcept(false) = default; // lies about noexcept, is trivial but throwing
};

template<bool SNoex, bool CNoex, bool ANoex>
struct S {
  S() = default;
  constexpr S(S&&) noexcept(CNoex) { } // not trivial
  constexpr S& operator=(S&& o) noexcept(ANoex) // not trivial
  { return *this; }

  friend constexpr
  void swap(S&, S&) noexcept(SNoex) {}
};

// n5008 inplace.vector.overview says for inplace_vector<T, 0>
// provides trivial copy/move/default cosntructpr regardless of T
struct Z
{
  Z(const Z&) = delete;
  Z& operator=(const Z&) = delete;
};

template<size_t N, typename T>
  constexpr std::inplace_vector<T, N>
  materialize(std::initializer_list<int> il)
  { 
    std::inplace_vector<T, N> res;
    for (int x : il)
      res.emplace_back(x);
    return res;
  }

static_assert(std::is_move_constructible_v<std::inplace_vector<int, 2>>);
static_assert(std::is_move_constructible_v<std::inplace_vector<X, 2>>);
static_assert(std::is_move_constructible_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_move_constructible_v<std::inplace_vector<D, 2>>);
static_assert(std::is_move_constructible_v<std::inplace_vector<U, 2>>);
// The operators are not constrained, as for any other container
static_assert(std::is_move_constructible_v<std::inplace_vector<Z, 2>>);

static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_nothrow_move_constructible_v<std::inplace_vector<X, 2>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<N<true, true>, 2>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<N<true, false>, 2>>);
static_assert(!std::is_nothrow_move_constructible_v<std::inplace_vector<N<false, true>, 2>>);
static_assert(!std::is_nothrow_move_constructible_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<D, 2>>);
static_assert(!std::is_nothrow_move_constructible_v<std::inplace_vector<U, 2>>);

static_assert(std::is_trivially_move_constructible_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_trivially_move_constructible_v<std::inplace_vector<X, 2>>);
static_assert(!std::is_trivially_move_constructible_v<std::inplace_vector<N<true, true>, 2>>);
// is_trivially_move_constructible_v checks destructor
static_assert(!std::is_trivially_move_constructible_v<std::inplace_vector<D, 2>>);
static_assert(std::is_trivially_move_constructible_v<std::inplace_vector<U, 2>>);

static_assert(std::is_move_assignable_v<std::inplace_vector<int, 2>>);
static_assert(std::is_move_assignable_v<std::inplace_vector<X, 2>>);
static_assert(std::is_move_assignable_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_move_assignable_v<std::inplace_vector<D, 2>>);
static_assert(std::is_move_assignable_v<std::inplace_vector<U, 2>>);
// The operators are not constrained, as for any other container
static_assert(std::is_move_assignable_v<std::inplace_vector<Z, 2>>);

static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_nothrow_move_assignable_v<std::inplace_vector<X, 2>>);
static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<N<true, true>, 2>>);
static_assert(!std::is_nothrow_move_assignable_v<std::inplace_vector<N<true, false>, 2>>);
static_assert(!std::is_nothrow_move_assignable_v<std::inplace_vector<N<false, true>, 2>>);
static_assert(!std::is_nothrow_move_assignable_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<D, 2>>);
static_assert(!std::is_nothrow_move_assignable_v<std::inplace_vector<U, 2>>);

static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_trivially_move_assignable_v<std::inplace_vector<X, 2>>);
static_assert(!std::is_trivially_move_assignable_v<std::inplace_vector<N<true, true>, 2>>);
// destructor is not trivial
static_assert(!std::is_trivially_move_assignable_v<std::inplace_vector<D, 2>>);
static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<U, 2>>);

static_assert(std::is_nothrow_swappable_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<X, 2>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<N<true, true>, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<N<true, false>, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<N<false, true>, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<S<true, true, true>, 2>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<S<true, true, false>, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<S<true, false, true>, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<S<true, false, false>, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<S<false, true, false>, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<S<false, false, false>, 2>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<D, 2>>);
static_assert(!std::is_nothrow_swappable_v<std::inplace_vector<U, 2>>);

static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<int, 0>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<X, 0>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<N<false, false>, 0>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<D, 0>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<U, 0>>);
static_assert(std::is_nothrow_move_constructible_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_trivially_move_constructible_v<std::inplace_vector<int, 0>>);
static_assert(std::is_trivially_move_constructible_v<std::inplace_vector<X, 0>>);
static_assert(std::is_trivially_move_constructible_v<std::inplace_vector<D, 0>>);
static_assert(std::is_trivially_move_constructible_v<std::inplace_vector<U, 0>>);
static_assert(std::is_trivially_move_constructible_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<int, 0>>);
static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<X, 0>>);
static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<N<false, false>, 0>>);
static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<D, 0>>);
static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<U, 0>>);
static_assert(std::is_nothrow_move_assignable_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<int, 0>>);
static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<X, 0>>);
static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<N<false, false>, 0>>);
static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<D, 0>>);
static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<U, 0>>);
static_assert(std::is_trivially_move_assignable_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_nothrow_swappable_v<std::inplace_vector<int, 0>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<X, 0>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<N<false, false>, 0>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<D, 0>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<U, 0>>);
static_assert(std::is_nothrow_swappable_v<std::inplace_vector<Z, 0>>);

template<typename T, size_t N>
constexpr bool
eq(const std::inplace_vector<T, N>& s, std::span<const T> o)
{ return std::ranges::equal(s, o); }

constexpr void
test_ctor()
{
  auto e0 = materialize<0, int>({});
  VERIFY( e0.empty() );
  auto e1 = materialize<0, X>({});
  VERIFY( e1.empty() );
  auto e2 = materialize<0, Z>({});
  VERIFY( e2.empty() );

  auto c0 = materialize<5, int>({});
  VERIFY( c0.empty() );

  auto c3 = materialize<5, int>({1, 2, 3});
  VERIFY( eq(c3, {1, 2, 3}) );

  auto c5 = materialize<5, int>({1, 2, 3, 4, 5});
  VERIFY( eq(c5, {1, 2, 3, 4, 5}) );

#ifdef __cpp_lib_constexpr_inplace_vector
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  auto x0 = materialize<3, X>({});
  VERIFY( x0.empty() );

  auto x2 = materialize<3, X>({1, 2});
  VERIFY( eq(x2, {1, 2}) );

  auto x3 = materialize<3, X>({1, 2, 3});
  VERIFY( eq(x3, {1, 2, 3}) );
}

constexpr void
test_assign()
{
  std::inplace_vector<int, 0> e0;
  e0 = materialize<0, int>({});
  VERIFY( e0.empty() );
  std::inplace_vector<X, 0> e1;
  e1 = materialize<0, X>({});
  VERIFY( e1.empty() );
  std::inplace_vector<Z, 0> e2;
  e2 = materialize<0, Z>({});
  VERIFY( e2.empty() );

  std::inplace_vector<int, 5> c;
  c = materialize<5, int>({});
  VERIFY( c.empty() );

  c = materialize<5, int>({1, 2, 3});
  VERIFY( eq(c, {1, 2, 3}) );
 
  c = materialize<5, int>({1, 2, 3, 4, 5});
  VERIFY( eq(c, {1, 2, 3, 4, 5}) );

  c = materialize<5, int>({4, 5});
  VERIFY( eq(c, {4, 5}) );

  c = materialize<5, int>({});
  VERIFY( c.empty() );

#ifdef __cpp_lib_constexpr_inplace_vector
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  std::inplace_vector<X, 5> x;
  x = materialize<5, X>({});
  VERIFY( x.empty() );

  x = materialize<5, X>({1, 2, 3});
  VERIFY( eq(x, {1, 2, 3}) );
 
  x = materialize<5, X>({1, 2, 3, 4, 5});
  VERIFY( eq(x, {1, 2, 3, 4, 5}) );

  x = materialize<5, X>({4, 5});
  VERIFY( eq(x, {4, 5}) );

  x = materialize<5, X>({});
  VERIFY( x.empty() );
}

constexpr void
test_swap()
{
  std::inplace_vector<int, 0> e0a, e0b;
  swap(e0a, e0b);
  VERIFY( e0a.empty() );
  VERIFY( e0b.empty() );
  e0a.swap(e0b);
  VERIFY( e0a.empty() );
  VERIFY( e0b.empty() );
 
  std::inplace_vector<X, 0> e1a, e1b;
  swap(e1a, e1b);
  VERIFY( e1a.empty() );
  VERIFY( e1b.empty() );
  e1a.swap(e1b);
  VERIFY( e1a.empty() );
  VERIFY( e1b.empty() );

  std::inplace_vector<Z, 0> e2a, e2b;
  swap(e2a, e2b);
  VERIFY( e2a.empty() );
  VERIFY( e2b.empty() );
  e2a.swap(e2b);
  VERIFY( e2a.empty() );
  VERIFY( e2b.empty() );

  std::inplace_vector<int, 5> c0;
  std::inplace_vector<int, 5> c3{1, 2, 3};
  std::inplace_vector<int, 5> c5{1, 2, 3, 4, 5};

  swap(c0, c3);
  VERIFY( c3.empty() );
  VERIFY( eq(c0, {1, 2, 3}) );
  c0.swap(c3);
  VERIFY( c0.empty() );
  VERIFY( eq(c3, {1, 2, 3}) );

  swap(c3, c5);
  VERIFY( eq(c5, {1, 2, 3}) );
  VERIFY( eq(c3, {1, 2, 3, 4, 5}) );
  c5.swap(c3);
  VERIFY( eq(c3, {1, 2, 3}) );
  VERIFY( eq(c5, {1, 2, 3, 4, 5}) );

#ifdef __cpp_lib_constexpr_inplace_vector
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  std::inplace_vector<X, 5> x0;
  std::inplace_vector<X, 5> x3 = {1, 2, 3};
  std::inplace_vector<X, 5> x5 = {1, 2, 3, 4, 5};

  swap(x0, x3);
  VERIFY( x3.empty() );
  VERIFY( eq(x0, {1, 2, 3}) );
  x0.swap(x3);
  VERIFY( x0.empty() );
  VERIFY( eq(x3, {1, 2, 3}) );

  swap(x3, x5);
  VERIFY( eq(x5, {1, 2, 3}) );
  VERIFY( eq(x3, {1, 2, 3, 4, 5}) );
  x5.swap(x3);
  VERIFY( eq(x3, {1, 2, 3}) );
  VERIFY( eq(x5, {1, 2, 3, 4, 5}) );
}

constexpr auto e0 = materialize<0, int>({});
constexpr auto e1 = materialize<0, X>({});
constexpr auto e2 = materialize<0, Z>({});

constexpr auto t1 = materialize<3, int>({});
constexpr auto t2 = materialize<3, int>({1, 2});
constexpr auto t3 = materialize<3, int>({11, 22, 33});

int main()
{
  auto tests = [] {
    test_ctor();
    test_assign();
    test_swap();
    return true;
  };

  tests();
  constexpr bool _ = tests();
}
