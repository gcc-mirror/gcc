// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <chrono>

using std::chrono::time_zone;

static_assert( std::is_move_constructible_v<time_zone> );
static_assert( std::is_move_assignable_v<time_zone> );

static_assert( ! std::is_default_constructible_v<time_zone> );
static_assert( ! std::is_copy_constructible_v<time_zone> );
static_assert( ! std::is_copy_assignable_v<time_zone> );

extern const time_zone* tz;

static_assert( std::is_same_v<decltype(tz->name()), std::string_view> );
static_assert( noexcept(tz->name()) );

static_assert( std::is_same_v<decltype(*tz == *tz), bool> );
static_assert( noexcept(*tz == *tz) );

static_assert( std::is_same_v<decltype(*tz <=> *tz), std::strong_ordering> );
static_assert( noexcept(*tz <=> *tz) );
