// { dg-do run { target c++26 } }

#include <inplace_vector>
#include <ranges>
#include <testsuite_hooks.h>

struct X
{
  X() = default;
  constexpr X(int p) : v(p) {}
  constexpr X(const X& o) : v(o.v) { } // not trivial
  constexpr X& operator=(const X& o) // not trivial
  { v = o.v; return *this; }

  int v;

  friend auto operator<=>(const X&, const X&) = default;
};

template<bool CNoex, bool ANoex>
struct N
{
  N() = default;
  constexpr N(const N&) noexcept(CNoex) { } // not trivial
  constexpr N& operator=(const N& o) noexcept(ANoex) // not trivial
  { return *this; }
};

struct D
{
  D() = default;
  D(const D&) = default;
  D& operator=(const D&) = default;
  ~D() {} // not trivially destructible
};

struct U
{
  U() = default;
  U(const U&) noexcept(false) = default; // lies about noexcept, is trivial but throwing
  U& operator=(const U&) noexcept(false) = default; // lies about noexcept, is trivial but throwing
};

// n5008 inplace.vector.overview p5 says for inplace_vector<T, 0>
// provides trivial copy/move/default cosntructpr regardless of T
struct Z
{
  Z(Z&&) = delete;
  Z& operator=(Z&&) = delete;
};

template<size_t N, typename T>
  constexpr std::inplace_vector<T, N> const&
  materialize(std::inplace_vector<T, N> const& r)
  { return r; }

static_assert(std::is_copy_constructible_v<std::inplace_vector<int, 2>>);
static_assert(std::is_copy_constructible_v<std::inplace_vector<X, 2>>);
static_assert(std::is_copy_constructible_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_copy_constructible_v<std::inplace_vector<D, 2>>);
static_assert(std::is_copy_constructible_v<std::inplace_vector<U, 2>>);
// The operators are not constrained, as for any other container
static_assert(std::is_copy_constructible_v<std::inplace_vector<Z, 2>>);

// conditional noexcept here is libstdc++ extension,
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_nothrow_copy_constructible_v<std::inplace_vector<X, 2>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<N<true, true>, 2>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<N<true, false>, 2>>);
static_assert(!std::is_nothrow_copy_constructible_v<std::inplace_vector<N<false, true>, 2>>);
static_assert(!std::is_nothrow_copy_constructible_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<D, 2>>);
static_assert(!std::is_nothrow_copy_constructible_v<std::inplace_vector<U, 2>>);

static_assert(std::is_trivially_copy_constructible_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_trivially_copy_constructible_v<std::inplace_vector<X, 2>>);
static_assert(!std::is_trivially_copy_constructible_v<std::inplace_vector<N<true, true>, 2>>);
// is_trivially_copy_constructible_v checks destructor
static_assert(!std::is_trivially_copy_constructible_v<std::inplace_vector<D, 2>>);
static_assert(std::is_trivially_copy_constructible_v<std::inplace_vector<U, 2>>);

static_assert(std::is_copy_assignable_v<std::inplace_vector<int, 2>>);
static_assert(std::is_copy_assignable_v<std::inplace_vector<X, 2>>);
static_assert(std::is_copy_assignable_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_copy_assignable_v<std::inplace_vector<D, 2>>);
static_assert(std::is_copy_assignable_v<std::inplace_vector<U, 2>>);
// The operators are not constrained, as for any other container
static_assert(std::is_copy_assignable_v<std::inplace_vector<Z, 2>>);

static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_nothrow_copy_assignable_v<std::inplace_vector<X, 2>>);
static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<N<true, true>, 2>>);
static_assert(!std::is_nothrow_copy_assignable_v<std::inplace_vector<N<true, false>, 2>>);
static_assert(!std::is_nothrow_copy_assignable_v<std::inplace_vector<N<false, true>, 2>>);
static_assert(!std::is_nothrow_copy_assignable_v<std::inplace_vector<N<false, false>, 2>>);
static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<D, 2>>);
static_assert(!std::is_nothrow_copy_assignable_v<std::inplace_vector<U, 2>>);

// conditional noexcept here is libstdc++ extension,
static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_trivially_copy_assignable_v<std::inplace_vector<X, 2>>);
static_assert(!std::is_trivially_copy_assignable_v<std::inplace_vector<N<true, true>, 2>>);
// destructor is not trivial
static_assert(!std::is_trivially_copy_assignable_v<std::inplace_vector<D, 2>>);
static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<U, 2>>);

static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<int, 0>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<X, 0>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<N<false, false>, 0>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<D, 0>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<U, 0>>);
static_assert(std::is_nothrow_copy_constructible_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_trivially_copy_constructible_v<std::inplace_vector<int, 0>>);
static_assert(std::is_trivially_copy_constructible_v<std::inplace_vector<X, 0>>);
static_assert(std::is_trivially_copy_constructible_v<std::inplace_vector<D, 0>>);
static_assert(std::is_trivially_copy_constructible_v<std::inplace_vector<U, 0>>);
static_assert(std::is_trivially_copy_constructible_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<int, 0>>);
static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<X, 0>>);
static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<N<false, false>, 0>>);
static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<D, 0>>);
static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<U, 0>>);
static_assert(std::is_nothrow_copy_assignable_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<int, 0>>);
static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<X, 0>>);
static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<N<false, false>, 0>>);
static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<D, 0>>);
static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<U, 0>>);
static_assert(std::is_trivially_copy_assignable_v<std::inplace_vector<Z, 0>>);


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
    return true;
  };

  tests();
  constexpr bool _ = tests();
}
