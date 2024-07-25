// { dg-do compile { target c++11 } }

// P2968R2 Make std::ignore a first-class object.
// This is a C++26 change, but we treat it as a DR against C++11.

// C++26 [tuple.general]:
// In addition to being available via inclusion of the <tuple> header,
// ignore is available when <utility> is included.
#include <utility>

using ignore_type = std::remove_const<decltype(std::ignore)>::type;

#ifdef __cpp_lib_is_aggregate
static_assert( std::is_aggregate_v<ignore_type> );
#endif

static_assert( std::is_nothrow_default_constructible<ignore_type>::value, "" );
static_assert( std::is_nothrow_copy_constructible<ignore_type>::value, "" );
static_assert( std::is_nothrow_copy_assignable<ignore_type>::value, "" );

static_assert( std::is_nothrow_assignable<const ignore_type&, int>::value,
    "assignable from arbitrary types" );
static_assert( std::is_nothrow_assignable<const ignore_type&, long*>::value,
    "assignable from arbitrary types" );

constexpr ignore_type ignore;
constexpr ignore_type ignore_more(ignore);
constexpr ignore_type ignore_morer(ignore = ignore);
constexpr ignore_type ignore_morest(ignore = "");
