// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <chrono>

using std::chrono::tzdb_list;

static_assert( ! std::is_default_constructible_v<tzdb_list> );
static_assert( ! std::is_copy_constructible_v<tzdb_list> );
static_assert( ! std::is_copy_assignable_v<tzdb_list> );
static_assert( ! std::is_move_constructible_v<tzdb_list> );
static_assert( ! std::is_move_assignable_v<tzdb_list> );
static_assert( std::is_destructible_v<tzdb_list> );

using IterTraits = std::iterator_traits<tzdb_list::const_iterator>;

static_assert( std::is_same_v<IterTraits::iterator_category,
			      std::forward_iterator_tag> );
static_assert( std::is_same_v<IterTraits::value_type, std::chrono::tzdb> );
