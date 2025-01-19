// { dg-options "-Wdeprecated" }
// { dg-do compile { target c++17 } }

#include <variant>

struct S { }; // std::hash<S> is a disabled specialization.

// Test std::hash size

template<typename... Ts>
constexpr std::size_t hash_size = sizeof(std::hash<std::variant<Ts...>>);

#if _GLIBCXX_INLINE_VERSION
// For the unstable ABI the size should always be one.
template<std::size_t Size, typename... Ts>
constexpr bool check_hash_size = hash_size<Ts...> == 1;
#else
// For the default ABI, the std::hash specialization has sizeof...(Ts)
// base classes of types __hash_empty_base<remove_cv_t<Ts>>... and if
// the same type occurs more than once they must have unique addresses.
template<std::size_t Size, typename... Ts>
constexpr bool check_hash_size = hash_size<Ts...> == Size;
#endif

static_assert( check_hash_size<1, int> );
static_assert( check_hash_size<1, int, long, double> );
static_assert( check_hash_size<2, int, long, int> );
static_assert( check_hash_size<2, int, long, const int> );
static_assert( check_hash_size<3, int, int, const int> );

static_assert( check_hash_size<1, S> );
static_assert( check_hash_size<1, int, S> );
static_assert( check_hash_size<2, int, S, int> );
static_assert( check_hash_size<2, int, S, int, S> );
static_assert( check_hash_size<2, int, S, S, int> );
static_assert( check_hash_size<3, int, S, S, int, S> );

// For the default ABI this has two __hash_empty_base<int> base classes,
// for the unstable ABI it does not.
struct H
: std::hash<std::variant<int>>, std::hash<std::variant<long, int>>
{ };
static_assert( sizeof(H) == hash_size<int, int, long> );
// Likewise, even though one of the base classes is a disabled specialization.
struct HX
: std::hash<std::variant<int>>, std::hash<std::variant<S, int>>
{ };
static_assert( sizeof(HX) == hash_size<int, S, int> );
