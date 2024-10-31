// { dg-do compile { target c++17 } }

#include <memory>

struct S { }; // std::hash<S> is a disabled specialization.

template<typename T, typename D = std::default_delete<T>>
constexpr std::size_t hash_size = sizeof(std::hash<std::unique_ptr<T, D>>);

template<typename... Ts>
struct MultiHash : std::hash<std::unique_ptr<Ts>>...
{ };

#if _GLIBCXX_INLINE_VERSION
// For the unstable ABI the size should always be one.
template<std::size_t Size, typename... Ts>
constexpr bool check_hash_size = sizeof(MultiHash<Ts...>) == 1;
#else
// For the default ABI, each std::hash<std::unique_ptr<T,D >> specialization
// has a base class of type __hash_empty_base<D::pointer> and if
// the same type occurs more than once they must have unique addresses.
template<std::size_t Size, typename... Ts>
constexpr bool check_hash_size = sizeof(MultiHash<Ts...>) == Size;
#endif

// All these types have distinct D::pointer types, so no duplicate base classes
static_assert( check_hash_size<1, int>, "" );
static_assert( check_hash_size<1, int, long, double>, "" );
static_assert( check_hash_size<1, int, const int>, "" );
static_assert( check_hash_size<1, int, long, const int>, "" );
// Likewise for these disabled specializations:
static_assert( check_hash_size<1, S>, "" );
static_assert( check_hash_size<1, int, S>, "" );
static_assert( check_hash_size<1, int, S, const int>, "" );
static_assert( check_hash_size<1, int, S, const int, const S>, "" );
static_assert( check_hash_size<1, int, S, const S, const int>, "" );

// But this has two base classes of type __hash_empty_base<int*>:
static_assert( check_hash_size<2, int, int[]>, "" );
static_assert( check_hash_size<2, int, int[], const int>, "" );
// And this has two base classes of type __hash_not_enabled<S*>:
static_assert( check_hash_size<2, S, S[]>, "" );
static_assert( check_hash_size<2, S, S[], const S>, "" );

struct Del : std::default_delete<int> { };
using P = std::unique_ptr<int>;
using PD = std::unique_ptr<int, Del>;
using PC = std::unique_ptr<int, std::default_delete<const int>>;
using PA = std::unique_ptr<int[]>;
struct HashClash
: std::hash<P>, std::hash<PD>, std::hash<PC>, std::hash<PA>
{ };
#if _GLIBCXX_INLINE_VERSION
static_assert(sizeof(HashClash) == 1, "No duplicate bases for unstable ABI");
#else
static_assert(sizeof(HashClash) == 4, "four __hash_empty_base<int*> bases");
#endif

struct Del2 : std::default_delete<const int> { using pointer = const int*; };
using PD2 = std::unique_ptr<int, Del2>;
struct Hash2
: std::hash<PD>, std::hash<PD2>
{ };
static_assert(sizeof(Hash2) == 1, "No duplicate bases");
