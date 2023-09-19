// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <functional>

#ifndef __cpp_lib_invoke_r
# error Feature-test macro for invoke_r is missing in <functional>
#elif __cpp_lib_invoke_r < 202106L
# error Feature-test macro for invoke_r has the wrong value in <functional>
#endif

constexpr int sq(int i) { return i * i; }

template<typename Val, typename Expected>
constexpr bool chk(Val&& val, Expected&& exp)
{
  return std::is_same_v<Val, Expected> && val == exp;
}

void
test01()
{
  static_assert( chk( std::invoke(sq, 2), 4 ) );
  static_assert( chk( std::invoke_r<int>(sq, 3), 9 ) );
  static_assert( chk( std::invoke_r<char>(sq, 4), '\x10' ) );
}

struct abstract {
  virtual ~abstract() = 0;
  void operator()() noexcept;
};

static_assert( noexcept(std::invoke(std::declval<abstract>())),
    "It should be possible to use abstract types with INVOKE" );

static_assert( noexcept(std::invoke_r<void>(std::declval<abstract>())),
    "It should be possible to use abstract types with INVOKE<R>" );

struct F {
  void operator()() &;
  void operator()() && noexcept;
  int operator()(int);
  double* operator()(int, int) noexcept;
};
struct D { D(void*); };

static_assert( !noexcept(std::invoke(std::declval<F&>())) );
static_assert( noexcept(std::invoke(std::declval<F>())) );
static_assert( !noexcept(std::invoke(std::declval<F>(), 1)) );
static_assert( noexcept(std::invoke(std::declval<F>(), 1, 2)) );

static_assert( !noexcept(std::invoke_r<void>(std::declval<F&>())) );
static_assert( noexcept(std::invoke_r<void>(std::declval<F>())) );
static_assert( !noexcept(std::invoke_r<int>(std::declval<F>(), 1)) );
static_assert( !noexcept(std::invoke_r<void>(std::declval<F>(), 1)) );
static_assert( !noexcept(std::invoke_r<long>(std::declval<F>(), 1)) );
static_assert( noexcept(std::invoke_r<void>(std::declval<F>(), 1, 2)) );
static_assert( noexcept(std::invoke_r<void*>(std::declval<F>(), 1, 2)) );
static_assert( !noexcept(std::invoke_r<D>(std::declval<F>(), 1, 2)) );
