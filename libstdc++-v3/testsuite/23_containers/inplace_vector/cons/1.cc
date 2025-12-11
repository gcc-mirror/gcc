// { dg-do run { target c++26 } }

#include <inplace_vector>
#include <testsuite_hooks.h>

struct X
{
  constexpr X() { } // not trivially default constructible
};

struct N
{
  constexpr N() noexcept { } // not trivially default constructible
};

struct D
{
  ~D() {} // not trivially destructible
};

struct U
{
  U() noexcept(false) = default; // lies about noexcept
};

// n5008 inplace.vector.overview says for inplace_vector<T, 0>
// provides trivial copy/move/default constructor regardless of T
struct Z
{
  constexpr Z(int) {}
  Z() = delete;
};

static_assert(std::is_default_constructible_v<std::inplace_vector<int, 2>>);
static_assert(std::is_default_constructible_v<std::inplace_vector<X, 2>>);
static_assert(std::is_default_constructible_v<std::inplace_vector<N, 2>>);
static_assert(std::is_default_constructible_v<std::inplace_vector<D, 2>>);
static_assert(std::is_default_constructible_v<std::inplace_vector<U, 2>>);
// The operators are not constrained, as for any other container
static_assert(std::is_default_constructible_v<std::inplace_vector<Z, 2>>);

static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<int, 2>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<X, 2>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<N, 2>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<D, 2>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<U, 2>>);

// Needs to set size to zero, not trivial
static_assert(!std::is_trivially_default_constructible_v<std::inplace_vector<int, 2>>);
static_assert(!std::is_trivially_default_constructible_v<std::inplace_vector<X, 2>>);
static_assert(!std::is_trivially_default_constructible_v<std::inplace_vector<N, 2>>);
static_assert(!std::is_trivially_default_constructible_v<std::inplace_vector<D, 2>>);
static_assert(!std::is_trivially_default_constructible_v<std::inplace_vector<U, 2>>);

#if !_GLIBCXX_DEBUG
static_assert(std::is_trivially_destructible_v<std::inplace_vector<int, 2>>);
static_assert(std::is_trivially_destructible_v<std::inplace_vector<X, 2>>);
static_assert(std::is_trivially_destructible_v<std::inplace_vector<N, 2>>);
static_assert(!std::is_trivially_destructible_v<std::inplace_vector<D, 2>>);
static_assert(std::is_trivially_destructible_v<std::inplace_vector<U, 2>>);
#endif

static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<int, 0>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<X, 0>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<N, 0>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<D, 0>>);
static_assert(std::is_nothrow_default_constructible_v<std::inplace_vector<U, 0>>);

// Size is always zero, so trivial
static_assert(std::is_trivially_default_constructible_v<std::inplace_vector<int, 0>>);
static_assert(std::is_trivially_default_constructible_v<std::inplace_vector<X, 0>>);
static_assert(std::is_trivially_default_constructible_v<std::inplace_vector<N, 0>>);
static_assert(std::is_trivially_default_constructible_v<std::inplace_vector<D, 0>>);
static_assert(std::is_trivially_default_constructible_v<std::inplace_vector<U, 0>>);
static_assert(std::is_trivially_default_constructible_v<std::inplace_vector<Z, 0>>);

static_assert(std::is_trivially_destructible_v<std::inplace_vector<int, 0>>);
static_assert(std::is_trivially_destructible_v<std::inplace_vector<X, 0>>);
static_assert(std::is_trivially_destructible_v<std::inplace_vector<N, 0>>);
static_assert(std::is_trivially_destructible_v<std::inplace_vector<D, 0>>);
static_assert(std::is_trivially_destructible_v<std::inplace_vector<U, 0>>);

static_assert(std::is_empty_v<std::inplace_vector<int, 0>>);
static_assert(std::is_empty_v<std::inplace_vector<X, 0>>);
static_assert(std::is_empty_v<std::inplace_vector<N, 0>>);
static_assert(std::is_empty_v<std::inplace_vector<D, 0>>);
static_assert(std::is_empty_v<std::inplace_vector<U, 0>>);
static_assert(std::is_empty_v<std::inplace_vector<Z, 0>>);

constexpr void
test_default()
{
  std::inplace_vector<int, 5> c;
  VERIFY( c.size() == 0 );
  VERIFY( c.capacity() == 5 );
  VERIFY( c.empty() );
  VERIFY( c.begin() == c.end() );

  std::inplace_vector<int, 0> c0;
  VERIFY( c0.size() == 0 );
  VERIFY( c0.capacity() == 0 );
  VERIFY( c0.empty() );
  VERIFY( c0.begin() == c0.end() );

  std::inplace_vector<Z, 0> z0;
  VERIFY( z0.size() == 0 );
  VERIFY( z0.capacity() == 0 );
  VERIFY( z0.empty() );
  VERIFY( z0.begin() == z0.end() );

#ifdef __cpp_lib_constexpr_inplace_vector
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  std::inplace_vector<X, 5> cx;
  VERIFY( cx.size() == 0 );
  VERIFY( cx.capacity() == 5 );
  VERIFY( cx.empty() );
  VERIFY( cx.begin() == cx.end() );

  std::inplace_vector<X, 0> cx0;
  VERIFY( cx0.size() == 0 );
  VERIFY( cx0.capacity() == 0 );
  VERIFY( cx0.empty() );
  VERIFY( cx0.begin() == cx0.end() );
}

constexpr void
test_n()
{
  std::inplace_vector<int, 5> c(2);
  VERIFY( c.size() == 2 );
  VERIFY( c.capacity() == 5 );
  VERIFY( not c.empty() );
  VERIFY( c.begin() + 2 == c.end() );
  VERIFY( c[0] == 0 );
  VERIFY( c[1] == 0 );

  std::inplace_vector<int, 2> c2(2);
  VERIFY( c2.size() == 2 );
  VERIFY( c2.capacity() == 2 );
  VERIFY( not c2.empty() );
  VERIFY( c2.begin() + 2 == c2.end() );
  VERIFY( c2[0] == 0 );
  VERIFY( c2[1] == 0 );

  std::inplace_vector<int, 0> c0(0);
  VERIFY( c0.size() == 0 );
  VERIFY( c0.capacity() == 0 );
  VERIFY( c0.empty() );
  VERIFY( c0.begin() == c0.end() );

  std::inplace_vector<int, 2> c20(0);
  VERIFY( c20.size() == 0 );
  VERIFY( c20.capacity() == 2 );
  VERIFY( c20.empty() );
  VERIFY( c20.begin() == c20.end() );

  std::inplace_vector<Z, 0> z0(0);
  VERIFY( z0.size() == 0 );
  VERIFY( z0.capacity() == 0 );
  VERIFY( z0.empty() );
  VERIFY( z0.begin() == z0.end() );

#ifdef __cpp_exceptions
#ifndef __cpp_lib_constexpr_exceptions
  if not consteval {
#endif
    try
    {
      std::inplace_vector<int, 2> ct(3);
      VERIFY(false);
    }
    catch (std::bad_alloc const&)
    {
    }

    try
    {
      std::inplace_vector<int, 0> ct(1);
      VERIFY(false);
    }
    catch (std::bad_alloc const&)
    {
    }

#ifndef __cpp_lib_constexpr_exceptions
  }
#endif
#endif

#ifdef __cpp_lib_constexpr_inplace_vector
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  std::inplace_vector<X, 5> cx(3);
  VERIFY( cx.size() == 3 );
  VERIFY( cx.capacity() == 5 );
  VERIFY( not cx.empty() );
  VERIFY( cx.begin() + 3 == cx.end() );
  (void) cx[2];
}

constexpr void
test_n_val()
{
  std::inplace_vector<int, 5> c(2, 99);
  VERIFY( c.size() == 2 );
  VERIFY( c.capacity() == 5 );
  VERIFY( not c.empty() );
  VERIFY( c.begin() + 2 == c.end() );
  VERIFY( c[0] == 99 );
  VERIFY( c[1] == 99 );

  std::inplace_vector<int, 1> c1(1, 44);
  VERIFY( c1.size() == 1 );
  VERIFY( c1.capacity() == 1 );
  VERIFY( not c1.empty() );
  VERIFY( c1.begin() + 1 == c1.end() );
  VERIFY( c1[0] == 44 );

  std::inplace_vector<int, 0> c0(0, 33);
  VERIFY( c0.size() == 0 );
  VERIFY( c0.capacity() == 0 );
  VERIFY( c0.empty() );
  VERIFY( c0.begin() == c0.end() );

  std::inplace_vector<int, 2> c20(0, 22);
  VERIFY( c20.size() == 0 );
  VERIFY( c20.capacity() == 2 );
  VERIFY( c20.empty() );
  VERIFY( c20.begin() == c20.end() );

  std::inplace_vector<Z, 0> z0(0, 33);
  VERIFY( z0.size() == 0 );
  VERIFY( z0.capacity() == 0 );
  VERIFY( z0.empty() );
  VERIFY( z0.begin() == z0.end() );

#ifdef __cpp_exceptions
#ifndef __cpp_lib_constexpr_exceptions
  if not consteval {
#endif
    try
    {
      std::inplace_vector<int, 2> ct(3, 11);
      VERIFY(false);
    }
    catch (std::bad_alloc const&)
    {
    }

    try
    {
      std::inplace_vector<int, 0> ct(2, 11);
      VERIFY(false);
    }
    catch (std::bad_alloc const&)
    {
    }
#ifndef __cpp_lib_constexpr_exceptions
  }
#endif
#endif

#ifdef __cpp_lib_constexpr_inplace_vector
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  std::inplace_vector<X, 5> cx(4);
  VERIFY( cx.size() == 4 );
  VERIFY( cx.capacity() == 5 );
  VERIFY( not cx.empty() );
  VERIFY( cx.begin() + 4 == cx.end() );
  (void) cx[3];
}

constexpr void
test_initializer_list()
{
  std::inplace_vector<int, 5> c{22, 33};
  VERIFY( c.size() == 2 );
  VERIFY( c.capacity() == 5 );
  VERIFY( not c.empty() );
  VERIFY( c.begin() + 2 == c.end() );
  VERIFY( c[0] == 22 );
  VERIFY( c[1] == 33 );

  std::inplace_vector<int, 1> c1{44};
  VERIFY( c1.size() == 1 );
  VERIFY( c1.capacity() == 1 );
  VERIFY( not c1.empty() );
  VERIFY( c1.begin() + 1 == c1.end() );
  VERIFY( c1[0] == 44 );

  std::inplace_vector<int, 0> c0({});
  VERIFY( c0.size() == 0 );
  VERIFY( c0.capacity() == 0 );
  VERIFY( c0.empty() );
  VERIFY( c0.begin() == c0.end() );

  std::inplace_vector<int, 2> c20({});
  VERIFY( c20.size() == 0 );
  VERIFY( c20.capacity() == 2 );
  VERIFY( c20.empty() );
  VERIFY( c20.begin() == c20.end() );

  std::inplace_vector<Z, 0> z0({});
  VERIFY( z0.size() == 0 );
  VERIFY( z0.capacity() == 0 );
  VERIFY( z0.empty() );
  VERIFY( z0.begin() == z0.end() );

#ifdef __cpp_exceptions
#ifndef __cpp_lib_constexpr_exceptions
  if not consteval {
#endif
    try
    {
      std::inplace_vector<int, 2> ct{11, 22, 33};
      VERIFY(false);
    }
    catch (std::bad_alloc const&)
    {
    }

    try
    {
      std::inplace_vector<int, 0> ct{11, 22};
      VERIFY(false);
    }
    catch (std::bad_alloc const&)
    {
    }
#ifndef __cpp_lib_constexpr_exceptions
  }
#endif
#endif

#ifdef __cpp_lib_constexpr_inplace_vector
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  std::inplace_vector<X, 5> cx{X(), X(), X(), X()};
  VERIFY( cx.size() == 4 );
  VERIFY( cx.capacity() == 5 );
  VERIFY( not cx.empty() );
  VERIFY( cx.begin() + 4 == cx.end() );
  (void) cx[3];
}

constexpr std::inplace_vector<int, 0> e0;
constexpr std::inplace_vector<X, 0> e1;
constexpr std::inplace_vector<Z, 0> e2;

constexpr std::inplace_vector<int, 5> g1;
constexpr std::inplace_vector<int, 5> g2(2, 100);
constexpr std::inplace_vector<int, 5> g3 = g2;
constexpr std::inplace_vector<int, 5> g4{1, 2, 3};
constexpr std::inplace_vector<int, 5> g5 = [] {
  std::inplace_vector<int, 5> res;
  res = g3;
  return res;
}();

int main()
{
  auto tests = [] {
    test_default();
    test_n();
    test_n_val();
    test_initializer_list();
    return true;
  };

  tests();
  constexpr bool _ = tests();
}
