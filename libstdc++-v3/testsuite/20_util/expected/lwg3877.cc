// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

// LWG 3877. Incorrect constraints on const-qualified monadic overloads

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
concept Has_and_then = requires(Exp&& exp, F f) {
  std::forward<Exp>(exp).and_then(f);
};

using ExpiT1 = std::expected<int, T1>;
static_assert( Has_and_then<ExpiT1&, ExpiT1(int)> );
static_assert( Has_and_then<const ExpiT1&, ExpiT1(int)> );
static_assert( Has_and_then<ExpiT1&&, ExpiT1(int)> );
static_assert( Has_and_then<const ExpiT1&&, ExpiT1(int)> );

using ExpiT2 = std::expected<int, T2>;
static_assert( !Has_and_then<ExpiT2&, ExpiT2(int)> );
static_assert( !Has_and_then<const ExpiT2&, ExpiT2(int)> );
static_assert( !Has_and_then<ExpiT2&&, ExpiT2(int)> );
static_assert( !Has_and_then<const ExpiT2&&, ExpiT2(int)> );

using ExpiT3 = std::expected<int, T3>;
static_assert( Has_and_then<ExpiT3&, ExpiT3(int)> );
static_assert( !Has_and_then<const ExpiT3&, ExpiT3(int)> );
static_assert( Has_and_then<ExpiT3&&, ExpiT3(int)> ); // uses and_then(F) const &&
static_assert( Has_and_then<const ExpiT3&&, ExpiT3(int)> );

template<typename Exp, typename F>
concept Has_or_else = requires(Exp&& exp, F f) {
  std::forward<Exp>(exp).or_else(f);
};

using ExpT1i = std::expected<T1, int>;
static_assert( Has_or_else<ExpT1i&, ExpT1i(int)> );
static_assert( Has_or_else<const ExpT1i&, ExpT1i(int)> );
static_assert( Has_or_else<ExpT1i&&, ExpT1i(int)> );
static_assert( Has_or_else<const ExpT1i&&, ExpT1i(int)> );

using ExpT2i = std::expected<T2, int>;
static_assert( !Has_or_else<ExpT2i&, ExpT2i(int)> );
static_assert( !Has_or_else<const ExpT2i&, ExpT2i(int)> );
static_assert( !Has_or_else<ExpT2i&&, ExpT2i(int)> );
static_assert( !Has_or_else<const ExpT2i&&, ExpT2i(int)> );

using ExpT3i = std::expected<T3, int>;
static_assert( Has_or_else<ExpT3i&, ExpT3i(int)> );
static_assert( !Has_or_else<const ExpT3i&, ExpT3i(int)> );
static_assert( Has_or_else<ExpT3i&&, ExpT3i(int)> ); // uses or_else(F) const &&
static_assert( Has_or_else<const ExpT3i&&, ExpT3i(int)> );

template<typename Exp, typename F>
concept Has_transform = requires(Exp&& exp, F f) {
  std::forward<Exp>(exp).transform(f);
};

static_assert( Has_transform<ExpiT1&, int(int)> );
static_assert( Has_transform<const ExpiT1&, int(int)> );
static_assert( Has_transform<ExpiT1&&, int(int)> );
static_assert( Has_transform<const ExpiT1&&, int(int)> );

static_assert( !Has_transform<ExpiT2&, int(int)> );
static_assert( !Has_transform<const ExpiT2&, int(int)> );
static_assert( !Has_transform<ExpiT2&&, int(int)> );
static_assert( !Has_transform<const ExpiT2&&, int(int)> );

static_assert( Has_transform<ExpiT3&, int(int)> );
static_assert( !Has_transform<const ExpiT3&, int(int)> );
static_assert( Has_transform<ExpiT3&&, int(int)> ); // uses transform(F) const &&
static_assert( Has_transform<const ExpiT3&&, int(int)> );

template<typename Exp, typename F>
concept Has_transform_error = requires(Exp&& exp, F f) {
  std::forward<Exp>(exp).transform_error(f);
};

static_assert( Has_transform_error<ExpT1i&, int(int)> );
static_assert( Has_transform_error<const ExpT1i&, int(int)> );
static_assert( Has_transform_error<ExpT1i&&, int(int)> );
static_assert( Has_transform_error<const ExpT1i&&, int(int)> );

static_assert( !Has_transform_error<ExpT2i&, int(int)> );
static_assert( !Has_transform_error<const ExpT2i&, int(int)> );
static_assert( !Has_transform_error<ExpT2i&&, int(int)> );
static_assert( !Has_transform_error<const ExpT2i&&, int(int)> );

static_assert( Has_transform_error<ExpT3i&, int(int)> );
static_assert( !Has_transform_error<const ExpT3i&, int(int)> );
static_assert( Has_transform_error<ExpT3i&&, int(int)> ); // uses transform_error(F) const &&
static_assert( Has_transform_error<const ExpT3i&&, int(int)> );

// std::expected<cv void, E>

using ExpvT1 = std::expected<void, T1>;
static_assert( Has_and_then<ExpvT1&, ExpvT1()> );
static_assert( Has_and_then<const ExpvT1&, ExpvT1()> );
static_assert( Has_and_then<ExpvT1&&, ExpvT1()> );
static_assert( Has_and_then<const ExpvT1&&, ExpvT1()> );

using ExpvT2 = std::expected<void, T2>;
static_assert( !Has_and_then<ExpvT2&, ExpvT2()> );
static_assert( !Has_and_then<const ExpvT2&, ExpvT2()> );
static_assert( !Has_and_then<ExpvT2&&, ExpvT2()> );
static_assert( !Has_and_then<const ExpvT2&&, ExpvT2()> );

using ExpvT3 = std::expected<void, T3>;
static_assert( Has_and_then<ExpvT3&, ExpvT3()> );
static_assert( !Has_and_then<const ExpvT3&, ExpvT3()> );
static_assert( Has_and_then<ExpvT3&&, ExpvT3()> ); // uses and_then(F) const &&
static_assert( Has_and_then<const ExpvT3&&, ExpvT3()> );

using Expvi = std::expected<void, int>;
static_assert( Has_or_else<Expvi&, Expvi(int)> );
static_assert( Has_or_else<const Expvi&, Expvi(int)> );
static_assert( Has_or_else<Expvi&&, Expvi(int)> );
static_assert( Has_or_else<const Expvi&&, Expvi(int)> );

static_assert( Has_transform<ExpvT1&, int()> );
static_assert( Has_transform<const ExpvT1&, int()> );
static_assert( Has_transform<ExpvT1&&, int()> );
static_assert( Has_transform<const ExpvT1&&, int()> );

static_assert( !Has_transform<ExpvT2&, int()> );
static_assert( !Has_transform<const ExpvT2&, int()> );
static_assert( !Has_transform<ExpvT2&&, int()> );
static_assert( !Has_transform<const ExpvT2&&, int()> );

static_assert( Has_transform<ExpvT3&, int()> );
static_assert( !Has_transform<const ExpvT3&, int()> );
static_assert( Has_transform<ExpvT3&&, int()> ); // uses transform(F) const &&
static_assert( Has_transform<const ExpvT3&&, int()> );

static_assert( Has_transform_error<Expvi&, int(int)> );
static_assert( Has_transform_error<const Expvi&, int(int)> );
static_assert( Has_transform_error<Expvi&&, int(int)> );
static_assert( Has_transform_error<const Expvi&&, int(int)> );
