// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <chrono>

using namespace std::chrono;

static_assert( std::is_default_constructible_v<zoned_time<seconds>> );
static_assert( std::is_copy_constructible_v<zoned_time<seconds>> );
static_assert( std::is_copy_assignable_v<zoned_time<seconds>> );
static_assert( std::is_move_constructible_v<zoned_time<seconds>> );
static_assert( std::is_move_assignable_v<zoned_time<seconds>> );
static_assert( std::is_destructible_v<zoned_time<seconds>> );

static_assert( std::is_same_v<zoned_time<seconds>::duration, seconds> );
static_assert( std::is_same_v<zoned_time<nanoseconds>::duration, nanoseconds> );
static_assert( std::is_same_v<zoned_time<minutes>::duration, seconds> );

extern zoned_time<minutes> z;
static_assert( std::is_same_v<decltype(z == z), bool> );

// requires zoned_traits<time_zone*>::default_zone().
static_assert( ! std::is_default_constructible_v<zoned_time<seconds, time_zone*>> );
// requires zoned_traits<time_zone*>::locate_zone(string_view).
static_assert( ! std::is_constructible_v<zoned_time<seconds, time_zone*>,
					 std::string_view> );
