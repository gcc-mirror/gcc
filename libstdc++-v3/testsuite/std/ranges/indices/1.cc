// { dg-do run { target c++26 } }

#include <testsuite_hooks.h>

#include <ranges>
#include <type_traits>
#include <stddef.h>

template <typename T>
constexpr bool test(T n) {
  auto indices_view = std::ranges::views::indices(n);
  static_assert(
      std::is_same_v<T, std::ranges::range_value_t<decltype(indices_view)>>);
  static_assert(noexcept(std::ranges::views::indices(n)));

  VERIFY(indices_view.size() == n);
  for (T i = 0; i < n; ++i) VERIFY(indices_view[i] == i);

  return true;
}

int main() {
  VERIFY(test<int>(41));
  static_assert(test<int>(41));
  VERIFY(test<short>(42));
  static_assert(test<short>(42));
  VERIFY(test<long>(43));
  static_assert(test<long>(43));
  VERIFY(test<size_t>(44));
  static_assert(test<size_t>(44));
}

template<typename T>
constexpr size_t test_wider(T n)
{
  // If indices(n) works, try again with ranges::distance(indices(n)),
  // which will be a wider type, until we get to an unsupported type.
  // This verifies that indices(n) is SFINAE-friendly, because otherwise we
  // would get a hard error outside the immediate context checked by requires.
  if constexpr (requires { std::views::indices(n); })
    return test_wider(std::ranges::distance(std::views::indices(n)));
  return sizeof(T);
}

static_assert(test_wider(0) > sizeof(long long));
