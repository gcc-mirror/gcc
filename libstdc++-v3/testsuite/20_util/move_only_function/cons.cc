// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <functional>

#ifndef __cpp_lib_move_only_function
# error "Feature-test macro for move_only_function missing in <functional>"
#elif __cpp_lib_move_only_function != 202110L
# error "Feature-test macro for move_only_function has wrong value in <functional>"
#endif

using std::move_only_function;

using std::is_constructible_v;
using std::is_copy_constructible_v;
using std::is_nothrow_default_constructible_v;
using std::is_nothrow_move_constructible_v;
using std::is_nothrow_constructible_v;
using std::nullptr_t;
using std::in_place_type_t;

static_assert( is_nothrow_default_constructible_v<move_only_function<void()>> );
static_assert( is_nothrow_constructible_v<move_only_function<void()>, nullptr_t> );
static_assert( is_nothrow_move_constructible_v<move_only_function<void()>> );
static_assert( ! is_copy_constructible_v<move_only_function<void()>> );

static_assert( is_constructible_v<move_only_function<void()>, void()> );
static_assert( is_constructible_v<move_only_function<void()>, void(&)()> );
static_assert( is_constructible_v<move_only_function<void()>, void(*)()> );
static_assert( is_constructible_v<move_only_function<void()>, int()> );
static_assert( is_constructible_v<move_only_function<void()>, int(&)()> );
static_assert( is_constructible_v<move_only_function<void()>, int(*)()> );
static_assert( ! is_constructible_v<move_only_function<void()>, void(int)> );
static_assert( is_constructible_v<move_only_function<void(int)>, void(int)> );

static_assert( is_constructible_v<move_only_function<void(int)>,
				  in_place_type_t<void(*)(int)>, void(int)> );

static_assert( is_constructible_v<move_only_function<void()>,
				  void() noexcept> );
static_assert( is_constructible_v<move_only_function<void() noexcept>,
				  void() noexcept> );
static_assert( ! is_constructible_v<move_only_function<void() noexcept>,
				    void() > );

struct Q
{
  void operator()() const &;
  void operator()() &&;
};

static_assert( is_constructible_v<move_only_function<void()>, Q> );
static_assert( is_constructible_v<move_only_function<void() const>, Q> );
static_assert( is_constructible_v<move_only_function<void() &>, Q> );
static_assert( is_constructible_v<move_only_function<void() const &>, Q> );
static_assert( is_constructible_v<move_only_function<void() &&>, Q> );
static_assert( is_constructible_v<move_only_function<void() const &&>, Q> );

struct R
{
  void operator()() &;
  void operator()() &&;
};

static_assert( is_constructible_v<move_only_function<void()>, R> );
static_assert( is_constructible_v<move_only_function<void()&>, R> );
static_assert( is_constructible_v<move_only_function<void()&&>, R> );
static_assert( ! is_constructible_v<move_only_function<void() const>, R> );
static_assert( ! is_constructible_v<move_only_function<void() const&>, R> );
static_assert( ! is_constructible_v<move_only_function<void() const&&>, R> );

// The following nothrow-constructible guarantees are a GCC extension,
// not required by the standard.

static_assert( is_nothrow_constructible_v<move_only_function<void()>, void()> );
static_assert( is_nothrow_constructible_v<move_only_function<void(int)>,
					  in_place_type_t<void(*)(int)>,
					  void(int)> );

// These types are all small and nothrow move constructible
struct F { void operator()(); };
struct G { void operator()() const; };
static_assert( is_nothrow_constructible_v<move_only_function<void()>, F> );
static_assert( is_nothrow_constructible_v<move_only_function<void()>, G> );
static_assert( is_nothrow_constructible_v<move_only_function<void() const>, G> );

struct H {
  H(int);
  H(int, int) noexcept;
  void operator()() noexcept;
};
static_assert( is_nothrow_constructible_v<move_only_function<void()>, H> );
static_assert( is_nothrow_constructible_v<move_only_function<void() noexcept>,
					  H> );
static_assert( ! is_nothrow_constructible_v<move_only_function<void() noexcept>,
					    in_place_type_t<H>, int> );
static_assert( is_nothrow_constructible_v<move_only_function<void() noexcept>,
					  in_place_type_t<H>, int, int> );

struct I {
  I(int, const char*);
  I(std::initializer_list<char>);
  int operator()() const noexcept;
};

static_assert( is_constructible_v<move_only_function<void()>,
				  std::in_place_type_t<I>,
				  int, const char*> );
static_assert( is_constructible_v<move_only_function<void()>,
				  std::in_place_type_t<I>,
				  std::initializer_list<char>> );

void
test_instantiation()
{
  // Instantiate the constructor bodies
  move_only_function<void()> f0;
  move_only_function<void()> f1(nullptr);
  move_only_function<void()> f2( I(1, "two") );
  move_only_function<void()> f3(std::in_place_type<I>, 3, "four");
  move_only_function<void()> f4(std::in_place_type<I>, // PR libstdc++/102825
				{ 'P', 'R', '1', '0', '2', '8', '2', '5'});
  auto f5 = std::move(f4);
  f4 = std::move(f5);
}
