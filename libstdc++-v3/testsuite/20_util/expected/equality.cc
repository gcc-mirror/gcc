// { dg-do compile { target c++23 } }

#include <expected>
#include <testsuite_hooks.h>

template<typename T, typename U>
concept Eq = requires(T t, U u) { t == u; };

static_assert(Eq<std::expected<int, long>, std::expected<short, unsigned>>);
static_assert(Eq<std::expected<void, long>, std::expected<void, unsigned>>);
// static_assert(!Eq<std::expected<void, long>, std::expected<short, unsigned>>);
static_assert(Eq<std::expected<int, long>, short>);
static_assert(!Eq<std::expected<void, long>, short>);
static_assert(Eq<std::expected<int, long>, std::unexpected<short>>);
static_assert(Eq<std::expected<void, long>, std::unexpected<short>>);

struct NotEqCmp
{
  constexpr bool operator==(int) const { return true; }
  bool operator==(NotEqCmp) const = delete;
};

constexpr bool
test_eq()
{
  std::expected<NotEqCmp, int> e1;
  VERIFY(e1 == 1);
  std::expected<int, int> e2;
  VERIFY(e2 == e2);
  VERIFY(e1 == e2);
  VERIFY(e1 != std::unexpected<int>(1));
  e1 = std::unexpected<int>(1);
  VERIFY(e1 == std::unexpected<int>(1));
  VERIFY(e1 != std::unexpected<int>(2));
  VERIFY(e1 != e2);

  std::expected<void, int> e3;
  VERIFY(e3 == e3);
  VERIFY(e3 != std::unexpected<int>(1));
  e3 = std::unexpected<int>(1);
  VERIFY(e3 == e3);
  VERIFY(e3 == std::unexpected<int>(1));
  VERIFY(e3 != std::unexpected<int>(2));

  return true;
}

static_assert( test_eq() );
