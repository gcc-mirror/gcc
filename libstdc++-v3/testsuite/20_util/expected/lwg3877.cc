// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <expected>

struct T1
{
};

struct T2
{
  T2(const T2&) = delete;
};

struct T3
{
  T3(const T3&) = delete;
  T3(T3&&) = delete;
  T3(T3&) { }
  T3(const T3&&) { }
};

template<typename Exp, typename F>
concept Has_or_else = requires(Exp&& exp, F f) {
  std::forward<Exp>(exp).or_else(f);
};

using E1 = std::expected<T1, int>;
static_assert( Has_or_else<E1&, E1(int)> );
static_assert( Has_or_else<const E1&, E1(int)> );
static_assert( Has_or_else<E1&&, E1(int)> );
static_assert( Has_or_else<const E1&&, E1(int)> );

using E2 = std::expected<T2, int>;
static_assert( !Has_or_else<E2&, E2(int)> );
static_assert( !Has_or_else<const E2&, E2(int)> );
static_assert( !Has_or_else<E2&&, E2(int)> );
static_assert( !Has_or_else<const E2&&, E2(int)> );

using E3 = std::expected<T3, int>;
static_assert( Has_or_else<E3&, E3(int)> );
static_assert( !Has_or_else<const E3&, E3(int)> );
static_assert( Has_or_else<E3&&, E3(int)> ); // uses or_else(F) const &&
static_assert( Has_or_else<const E3&&, E3(int)> );

template<typename Exp, typename F>
concept Has_transform_error = requires(Exp&& exp, F f) {
  std::forward<Exp>(exp).transform_error(f);
};

static_assert( Has_transform_error<E1&, int(int)> );
static_assert( Has_transform_error<const E1&, int(int)> );
static_assert( Has_transform_error<E1&&, int(int)> );
static_assert( Has_transform_error<const E1&&, int(int)> );

static_assert( !Has_transform_error<E2&, int(int)> );
static_assert( !Has_transform_error<const E2&, int(int)> );
static_assert( !Has_transform_error<E2&&, int(int)> );
static_assert( !Has_transform_error<const E2&&, int(int)> );

static_assert( Has_transform_error<E3&, int(int)> );
static_assert( !Has_transform_error<const E3&, int(int)> );
static_assert( Has_transform_error<E3&&, int(int)> ); // uses transform_error(F) const &&
static_assert( Has_transform_error<const E3&&, int(int)> );
