// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <utility>

#ifndef __cpp_lib_forward_like
# error "Feature-test macro for forward_like missing in <utility>"
#elif __cpp_lib_forward_like != 202207L
# error "Feature-test macro for forward_like has wrong value in <utility>"
#endif

template<typename T, typename U>
using forward_like_t = decltype(std::forward_like<T>(std::declval<U>()));

#if 0
using std::is_same_v;
#else
#include <concepts>
template<class T, class U> concept is_same_v = std::same_as<T, U>;
#endif

static_assert( is_same_v<forward_like_t<int, long>, long&&> );
static_assert( is_same_v<forward_like_t<int&, long>, long&> );
static_assert( is_same_v<forward_like_t<int&&, long>, long&&> );

static_assert( is_same_v<forward_like_t<const int, long>, const long&&> );
static_assert( is_same_v<forward_like_t<const int&, long>, const long&> );
static_assert( is_same_v<forward_like_t<const int&&, long>, const long&&> );

static_assert( is_same_v<forward_like_t<int, const long>, const long&&> );
static_assert( is_same_v<forward_like_t<int&, const long>, const long&> );
static_assert( is_same_v<forward_like_t<int&&, const long>, const long&&> );

static_assert( is_same_v<forward_like_t<const int, long&>, const long&&> );
static_assert( is_same_v<forward_like_t<const int&, long&>, const long&> );
static_assert( is_same_v<forward_like_t<const int&&, long&>, const long&&> );

static_assert( is_same_v<forward_like_t<int, const long&>, const long&&> );
static_assert( is_same_v<forward_like_t<int&, const long&>, const long&> );
static_assert( is_same_v<forward_like_t<int&&, const long&>, const long&&> );

static_assert( is_same_v<forward_like_t<const int, long&&>, const long&&> );
static_assert( is_same_v<forward_like_t<const int&, long&&>, const long&> );
static_assert( is_same_v<forward_like_t<const int&&, long&&>, const long&&> );

static_assert( is_same_v<forward_like_t<int, const long&&>, const long&&> );
static_assert( is_same_v<forward_like_t<int&, const long&&>, const long&> );
static_assert( is_same_v<forward_like_t<int&&, const long&&>, const long&&> );

static_assert( is_same_v<forward_like_t<volatile int, long>, long&&> );
static_assert( is_same_v<forward_like_t<volatile int&, long>, long&> );
static_assert( is_same_v<forward_like_t<volatile int&&, long>, long&&> );

static_assert( is_same_v<forward_like_t<const int, volatile long>,
			 const volatile long&&> );
static_assert( is_same_v<forward_like_t<const int&, volatile long>,
			 const volatile long&> );
static_assert( is_same_v<forward_like_t<const int&&, volatile long>,
			 const volatile long&&> );
