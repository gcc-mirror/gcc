// { dg-do run { target c++23 } }

// LWG 4222. 'expected' constructor from a single value missing a constraint

#include <expected>
#include <type_traits>
#include <testsuite_hooks.h>

struct T {
  explicit T(auto) {}
};
struct E {
  E(int) {}
};

struct V {
 explicit constexpr V(std::unexpect_t) {}
};

static_assert(!std::is_constructible_v<std::expected<T, E>, std::unexpect_t>);
static_assert(!std::is_constructible_v<std::expected<T, E>, std::unexpect_t &>);
static_assert(!std::is_constructible_v<std::expected<T, E>, std::unexpect_t &&>);
static_assert(!std::is_constructible_v<std::expected<T, E>, const std::unexpect_t>);
static_assert(!std::is_constructible_v<std::expected<T, E>, const std::unexpect_t &>);
static_assert(!std::is_constructible_v<std::expected<T, E>, const std::unexpect_t &&>);

constexpr bool test() {
  std::expected<V, int> e1(std::in_place, std::unexpect);
  VERIFY( e1.has_value() );
  std::expected<int, V> e2(std::unexpect, std::unexpect);
  VERIFY( !e2.has_value() );
  return true;
}

int main() {
  test();
  static_assert(test());
  return 0;
}
