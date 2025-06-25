// { dg-do compile { target c++26 } }

#include <concepts>
#include <format>
#include <iterator>
#include <optional>
#include <ranges>
#include <string_view>
#include <vector>

#include <testsuite_hooks.h>

template<typename O>
constexpr
void
test_range_concepts()
{
  static_assert(std::ranges::contiguous_range<O>);
  static_assert(std::ranges::sized_range<O>);
  static_assert(std::ranges::common_range<O>);
  static_assert(!std::ranges::borrowed_range<O>);

  // an optional<const T> is not assignable, and therefore does not satisfy ranges::view
  using T = typename O::value_type;
  constexpr bool is_const_opt = std::is_const_v<T>;
  static_assert(std::ranges::view<O> == !is_const_opt);
  static_assert(std::ranges::viewable_range<O> == !is_const_opt);
}

template<typename O>
constexpr
void
test_iterator_concepts()
{
  using T = typename O::value_type;
  using iterator = typename O::iterator;
  static_assert(std::contiguous_iterator<iterator>);
  static_assert(std::is_same_v<typename std::iterator_traits<iterator>::value_type, std::remove_cv_t<T>>);
  static_assert(std::is_same_v<std::iter_value_t<iterator>, std::remove_cv_t<T>>);
  static_assert(std::is_same_v<typename std::iterator_traits<iterator>::reference, T&>);
  static_assert(std::is_same_v<std::iter_reference_t<iterator>, T&>);

  using const_iterator = typename O::const_iterator;
  static_assert(std::contiguous_iterator<const_iterator>);
  static_assert(std::is_same_v<typename std::iterator_traits<const_iterator>::value_type, std::remove_cv_t<T>>);
  static_assert(std::is_same_v<std::iter_value_t<const_iterator>, std::remove_cv_t<T>>);
  static_assert(std::is_same_v<typename std::iterator_traits<const_iterator>::reference, const T&>);
  static_assert(std::is_same_v<std::iter_reference_t<const_iterator>, const T&>);
}

template<typename O>
constexpr
void
test_empty()
{
  O empty;
  VERIFY(!empty);
  VERIFY(empty.begin() == empty.end());
  VERIFY(std::as_const(empty).begin() == std::as_const(empty).end());
  VERIFY(std::ranges::empty(empty));
  VERIFY(std::ranges::empty(std::as_const(empty)));
  VERIFY(std::ranges::empty(empty | std::views::as_const));
  VERIFY(std::ranges::size(empty) == 0);
  VERIFY(std::ranges::size(std::as_const(empty)) == 0);

  size_t count = 0;
  for (const auto& x : empty)
    ++count;
  VERIFY(count == 0);
}

template<typename O, typename T>
constexpr
void
test_non_empty(const T& value)
{
  O non_empty = std::make_optional(value);
  VERIFY(non_empty);
  VERIFY(*non_empty == value);
  VERIFY(non_empty.begin() != non_empty.end());
  VERIFY(non_empty.begin() < non_empty.end());
  VERIFY(std::as_const(non_empty).begin() != std::as_const(non_empty).end());
  VERIFY(std::as_const(non_empty).begin() < std::as_const(non_empty).end());
  VERIFY(!std::ranges::empty(non_empty));
  VERIFY(!std::ranges::empty(std::as_const(non_empty)));
  VERIFY(!std::ranges::empty(non_empty | std::views::as_const));
  VERIFY(std::ranges::size(non_empty) == 1);
  VERIFY(std::ranges::size(std::as_const(non_empty)) == 1);

  size_t count = 0;
  for (const auto& x : non_empty)
    ++count;
  VERIFY(count == 1);

  if constexpr (!std::is_const_v<typename O::value_type>) {
    for (auto& x : non_empty)
      x = T{};
    VERIFY(non_empty);
    VERIFY(*non_empty == T{});
  }
}

template<typename T>
constexpr
void
test(const T& value)
{
  using O = std::optional<T>;
  test_range_concepts<O>();
  test_iterator_concepts<O>();
  test_empty<O>();
  test_non_empty<O>(value);
  static_assert(!std::formattable<O, char>);
  static_assert(!std::formattable<O, wchar_t>);
  static_assert(std::format_kind<O> == std::range_format::disabled);
}

constexpr
void
range_chain_example() // from P3168
{
  std::vector<int> v{2, 3, 4, 5, 6, 7, 8, 9, 1};
  auto test = [](int i) -> std::optional<int> {
    switch(i) {
    case 1:
    case 3:
    case 7:
    case 9:
      return i * 2;
    default:
      return {};
    }
  };

  auto result = v
    | std::views::transform(test)
    | std::views::filter([](auto x) { return bool(x); })
    | std::views::transform([](auto x){ return *x; })
    | std::ranges::to<std::vector>();

  bool ok = result == std::vector<int>{6, 14, 18, 2};
  VERIFY(ok);
}

constexpr
bool
all_tests()
{
  test(42);
  int i = 42;
  test(&i);
  test(std::string_view("test"));
  test(std::vector<int>{1, 2, 3, 4});
  test(std::optional<int>(42));
  test<const int>(42);

  range_chain_example();

  return true;
}

static_assert(all_tests());

