// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

// LWG 3938. Cannot use std::expected monadic ops with move-only error_type

#include <expected>
#include <testsuite_hooks.h>

struct MoveOnly {
  constexpr MoveOnly(int i) : i(i) { }
  constexpr MoveOnly(MoveOnly&&) = default;
  constexpr MoveOnly(const MoveOnly&& mo) : i(mo.i) { }
  constexpr bool operator==(const MoveOnly&) const = default;
  int i;
};

constexpr bool
test_and_then()
{
  auto fun = [](int i) { return std::expected<long, MoveOnly>(i); };

  std::expected<int, MoveOnly> good(9);
  std::expected<long, MoveOnly> e1 = std::move(good).and_then(fun);
  VERIFY( e1 == good );
  const auto& gooder = good;
  std::expected<long, MoveOnly> e2 = std::move(gooder).and_then(fun);
  VERIFY( e2 == gooder );

  std::expected<int, MoveOnly> bad(std::unexpect, 99);
  std::expected<long, MoveOnly> e3 = std::move(bad).and_then(fun);
  VERIFY( e3 == bad );
  const auto& badder = bad;
  std::expected<long, MoveOnly> e4 = std::move(badder).and_then(fun);
  VERIFY( e4 == badder );

  auto vun = [] { return std::expected<long, MoveOnly>(1); };
  std::expected<void, MoveOnly> vud;
  std::expected<long, MoveOnly> e5 = std::move(vud).and_then(vun);
  VERIFY( *e5 == 1 );
  const auto& vudder = vud;
  std::expected<long, MoveOnly> e6 = std::move(vudder).and_then(vun);
  VERIFY( *e6 == 1 );

  return true;
}

static_assert( test_and_then() );

constexpr bool
test_or_else()
{
  auto fun = [](const MoveOnly&& mo) { return std::expected<int, long>(mo.i); };

  std::expected<int, MoveOnly> good(9);
  std::expected<int, long> e1 = std::move(good).or_else(fun);
  VERIFY( e1 == good );
  const auto& gooder = good;
  std::expected<int, long> e2 = std::move(gooder).or_else(fun);
  VERIFY( e2 == gooder );

  std::expected<int, MoveOnly> bad(std::unexpect, 99);
  std::expected<int, long> e3 = std::move(bad).or_else(fun);
  VERIFY( *e3 == 99 );
  const auto& badder = bad;
  std::expected<int, long> e4 = std::move(badder).or_else(fun);
  VERIFY( *e4 == 99 );

  auto vun = [](const MoveOnly&& mo) { return std::expected<void, long>{}; };
  std::expected<void, MoveOnly> vud;
  std::expected<void, long> e5 = std::move(vud).or_else(vun);
  VERIFY( e5.has_value() );
  const auto& vudder = vud;
  std::expected<void, long> e6 = std::move(vudder).or_else(vun);
  VERIFY( e6.has_value() );

  return true;
}

static_assert( test_or_else() );

constexpr bool
test_transform()
{
  auto fun = [](int i) { return (long)i; };

  std::expected<int, MoveOnly> good(9);
  std::expected<long, MoveOnly> e1 = std::move(good).transform(fun);
  VERIFY( e1 == good );
  const auto& gooder = good;
  std::expected<long, MoveOnly> e2 = std::move(gooder).transform(fun);
  VERIFY( e2 == gooder );

  std::expected<int, MoveOnly> bad(std::unexpect, 99);
  std::expected<long, MoveOnly> e3 = std::move(bad).transform(fun);
  VERIFY( e3 == bad );
  const auto& badder = bad;
  std::expected<long, MoveOnly> e4 = std::move(badder).transform(fun);
  VERIFY( e4 == badder );

  auto vun = []() { return 1L; };
  std::expected<void, MoveOnly> vud;
  std::expected<long, MoveOnly> e5 = std::move(vud).transform(vun);
  VERIFY( *e5 == 1 );
  const auto& vudder = vud;
  std::expected<long, MoveOnly> e6 = std::move(vudder).transform(vun);
  VERIFY( *e6 == 1 );

  return true;
}

static_assert( test_transform() );

constexpr bool
test_transform_error()
{
  auto fun = [](const MoveOnly&& mo) { return (long)mo.i; };

  std::expected<int, MoveOnly> good(9);
  std::expected<int, long> e1 = std::move(good).transform_error(fun);
  VERIFY( e1 == good );
  const auto& gooder = good;
  std::expected<int, long> e2 = std::move(gooder).transform_error(fun);
  VERIFY( e2 == gooder );

  std::expected<int, MoveOnly> bad(std::unexpect, 99);
  std::expected<int, long> e3 = std::move(bad).transform_error(fun);
  VERIFY( e3.error() == 99 );
  const auto& badder = bad;
  std::expected<int, long> e4 = std::move(badder).transform_error(fun);
  VERIFY( e4.error() == 99 );

  std::expected<void, MoveOnly> vud(std::unexpect, 1);
  std::expected<void, long> e5 = std::move(vud).transform_error(fun);
  VERIFY( e5.error() == 1 );
  const auto& vudder = vud;
  std::expected<void, long> e6 = std::move(vudder).transform_error(fun);
  VERIFY( e6.error() == 1 );

  return true;
}

static_assert( test_transform_error() );
