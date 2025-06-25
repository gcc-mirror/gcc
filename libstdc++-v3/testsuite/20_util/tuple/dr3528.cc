// { dg-do compile { target c++17 } }

// LWG 3528. make_from_tuple can perform (the equivalent of) a C-style cast

#include <tuple>
#include <array>
#include <utility>

template<typename T, typename Tuple>
using make_t = decltype(std::make_from_tuple<T>(std::declval<Tuple>()));

template<typename T, typename Tuple, typename = void>
constexpr bool can_make = false;
template<typename T, typename Tuple>
constexpr bool can_make<T, Tuple, std::void_t<make_t<T, Tuple>>> = true;

static_assert( can_make<int, std::tuple<int>> );
static_assert( can_make<int, std::tuple<int>&> );
static_assert( can_make<int, const std::tuple<int>&> );
static_assert( can_make<int, std::array<short, 1>> );
static_assert( can_make<int, const std::array<short, 1>&&> );
static_assert( can_make<std::tuple<int, int>, std::pair<unsigned, long>> );
static_assert( can_make<std::pair<int, int>, std::array<int, 2>> );
static_assert( can_make<const int*, std::tuple<int*>> );
static_assert( can_make<void*, std::tuple<int*>> );
static_assert( can_make<int, std::tuple<>> );
static_assert( ! can_make<int, std::tuple<int, int>> );
static_assert( ! can_make<int, std::pair<short, char>> );
static_assert( ! can_make<int, std::pair<short, char>&> );
static_assert( ! can_make<int, std::tuple<const char*>> );
static_assert( ! can_make<int*, std::tuple<const int*>> );
static_assert( ! can_make<int*, std::tuple<void*>> );
static_assert( ! can_make<int, std::array<int, 2>> );
static_assert( ! can_make<void, std::tuple<>> );
static_assert( ! can_make<void, std::array<int, 1>> );

struct Two
{
  Two(const char*, int);
};

static_assert( can_make<Two, std::tuple<char*, unsigned>> );
static_assert( ! can_make<Two, std::tuple<const char*, int, int>> );
static_assert( can_make<Two, std::pair<const char*, long>> );
static_assert( ! can_make<Two, std::pair<int*, long>> );
static_assert( ! can_make<std::pair<int, int>, std::array<int, 3>> );
