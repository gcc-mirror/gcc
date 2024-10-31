// { dg-do compile { target c++17 } }

#include <optional>

struct S { }; // std::hash<S> is a disabled specialization.

template<typename T>
constexpr std::size_t hash_size = sizeof(std::hash<std::optional<T>>);

template<typename... Ts>
struct MultiHash : std::hash<std::optional<Ts>>...
{ };

#if _GLIBCXX_INLINE_VERSION
// For the unstable ABI the size should always be one.
template<std::size_t Size, typename... Ts>
constexpr bool check_hash_size = sizeof(MultiHash<Ts...>) == 1;
#else
// For the default ABI, each std::hash<std::optional<T>> specialization has
// a base class of type __hash_empty_base<remove_cv_t<T>> and if
// the same type occurs more than once they must have unique addresses.
template<std::size_t Size, typename... Ts>
constexpr bool check_hash_size = sizeof(MultiHash<Ts...>) == Size;
#endif

static_assert( check_hash_size<1, int> );
static_assert( check_hash_size<1, int, long, double> );
static_assert( check_hash_size<2, int, const int> );
static_assert( check_hash_size<2, int, long, const int> );

static_assert( check_hash_size<1, S> );
static_assert( check_hash_size<1, int, S> );
static_assert( check_hash_size<2, int, S, const int> );
static_assert( check_hash_size<2, int, S, const int, const S> );
static_assert( check_hash_size<2, int, S, const S, const int> );
