// { dg-do compile { target c++26 } }

#include <concepts>
#include <format>
#include <iterator>
#include <optional>
#include <ranges>
#include <string_view>
#include <vector>

#include <testsuite_hooks.h>

struct NonMovable
{
  constexpr NonMovable() {}
  constexpr NonMovable(int) {}

  NonMovable(NonMovable&&) = delete;
  NonMovable& operator=(NonMovable&&) = delete;

  friend bool operator==(NonMovable const&, NonMovable const&) = default;
};

struct NonAssignable
{
  NonAssignable() = default;
  NonAssignable(NonAssignable&&) = default;
  NonAssignable& operator=(NonAssignable&&) = delete;
  
  friend bool operator==(NonAssignable const&, NonAssignable const&) = default;
};

template<typename T>
constexpr
void
test_range_concepts()
{
  using O = std::optional<T>;
  static_assert(std::ranges::contiguous_range<O>);
  static_assert(std::ranges::sized_range<O>);
  static_assert(std::ranges::common_range<O>);

  // an optional<T&> is borrowed range
  constexpr bool is_ref_opt = std::is_reference_v<T>;
  static_assert(std::ranges::borrowed_range<O> == is_ref_opt);

  // for any T (including const U) such that optional<T> is not assignable,
  // it does not satisfy ranges::view
  constexpr bool is_opt_view = std::is_reference_v<T> || std::movable<T>;
  static_assert(std::ranges::view<O> == is_opt_view);
  static_assert(std::ranges::viewable_range<O> == is_opt_view);

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
}

template<typename O>
constexpr
void
test_const_iterator_concepts()
{
  using T = typename O::value_type;
  using const_iterator = typename O::const_iterator;
  static_assert(std::contiguous_iterator<const_iterator>);
  static_assert(std::is_same_v<typename std::iterator_traits<const_iterator>::value_type, std::remove_cv_t<T>>);
  static_assert(std::is_same_v<std::iter_value_t<const_iterator>, std::remove_cv_t<T>>);
  static_assert(std::is_same_v<typename std::iterator_traits<const_iterator>::reference, const T&>);
  static_assert(std::is_same_v<std::iter_reference_t<const_iterator>, const T&>);
}

template<typename T>
constexpr
void
test_empty()
{
  using O = std::optional<T>;
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

template<typename T>
constexpr
void
test_non_empty(const T& value)
{
  using O = std::optional<T>;
  using V = typename O::value_type;
  O non_empty(std::in_place, value);
  VERIFY(non_empty);
  if constexpr (!std::is_array_v<V>)
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

  if constexpr (std::is_move_assignable_v<V>) {
    for (auto& x : non_empty)
      x = V{};
    VERIFY(non_empty);
    VERIFY(*non_empty == V{});
  }
}

template<typename T>
constexpr
void
test(const T& value)
{
  using O = std::optional<T>;
  test_range_concepts<T>();
  test_iterator_concepts<O>();
  if constexpr (!std::is_reference_v<T>)
    test_const_iterator_concepts<O>();
  test_empty<T>();
  test_non_empty<T>(value);
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

template<typename T>
constexpr void test_not_range()
{
  static_assert(!requires { typename std::optional<T>::iterator; });
  static_assert(!requires(std::optional<T> o) { o.begin(); });
  static_assert(!requires(std::optional<T> o) { o.end(); });
};

template<typename T>
constexpr bool is_optional = false;

template<typename T>
constexpr bool is_optional<std::optional<T>> = true;

template<bool usesOptional, typename T, typename U = std::remove_cv_t<T>>
constexpr void test_as_const(std::type_identity_t<U> u)
{
  std::optional<T> o(std::in_place, std::forward<U>(u));
  auto cv = std::views::as_const(o);
  static_assert(is_optional<decltype(cv)> == usesOptional);
  static_assert(std::is_same_v<decltype(*cv.begin()), const std::remove_reference_t<T>&>);
  VERIFY(!std::ranges::empty(cv));
      
  std::optional<T> e;
  auto cve = std::views::as_const(e);
  static_assert(is_optional<decltype(cve)> == usesOptional);
  static_assert(std::is_same_v<decltype(*cve.begin()), const std::remove_reference_t<T>&>);
  VERIFY(std::ranges::empty(cve));
}

template<bool usesOptional, typename T, typename U = std::remove_cv_t<T>>
constexpr void
test_reverse(std::type_identity_t<U> u)
{
  std::optional<T> o(std::in_place, std::forward<U>(u));
  auto rv = std::views::reverse(o);
  static_assert(is_optional<decltype(rv)> == usesOptional);
  static_assert(std::is_same_v<decltype(*rv.begin()), T&>);
  VERIFY(!std::ranges::empty(rv));
      
  std::optional<T> e;
  auto rve = std::views::reverse(e);
  static_assert(is_optional<decltype(rve)> == usesOptional);
  static_assert(std::is_same_v<decltype(*rve.begin()), T&>);
  VERIFY(std::ranges::empty(rve));
}

template<bool usesOptional, typename T, typename U = std::remove_cv_t<T>>
constexpr void
test_take(std::type_identity_t<U> u)
{
  std::optional<T> o(std::in_place, std::forward<U>(u));
  auto tvp = std::views::take(o, 3);
  static_assert(is_optional<decltype(tvp)> == usesOptional);
  static_assert(std::is_same_v<decltype(*tvp.begin()), T&>);
  VERIFY(!std::ranges::empty(tvp));

  auto tvz = std::views::take(o, 0);
  static_assert(is_optional<decltype(tvz)> == usesOptional);
  static_assert(std::is_same_v<decltype(*tvz.begin()), T&>);
  VERIFY(std::ranges::empty(tvz));

  std::optional<T> e;
  auto tvep = std::views::take(e, 5);
  static_assert(is_optional<decltype(tvep)> == usesOptional);
  static_assert(std::is_same_v<decltype(*tvep.begin()), T&>);
  VERIFY(std::ranges::empty(tvep));

  auto tvez = std::views::take(e, 0);
  static_assert(is_optional<decltype(tvez)> == usesOptional);
  static_assert(std::is_same_v<decltype(*tvez.begin()), T&>);
  VERIFY(std::ranges::empty(tvez));
}

template<bool usesOptional, typename T, typename U = std::remove_cv_t<T>>
constexpr void
test_drop(std::type_identity_t<U> u)
{
  std::optional<T> o(std::in_place, std::forward<U>(u));
  auto dvp = std::views::drop(o, 3);
  static_assert(is_optional<decltype(dvp)> == usesOptional);
  static_assert(std::is_same_v<decltype(*dvp.begin()), T&>);
  VERIFY(std::ranges::empty(dvp));

  auto dvz = std::views::drop(o, 0);
  static_assert(is_optional<decltype(dvz)> == usesOptional);
  static_assert(std::is_same_v<decltype(*dvz.begin()), T&>);
  VERIFY(!std::ranges::empty(dvz));

  std::optional<T> e;
  auto dvep = std::views::drop(e, 5);
  static_assert(is_optional<decltype(dvep)> == usesOptional);
  static_assert(std::is_same_v<decltype(*dvep.begin()), T&>);
  VERIFY(std::ranges::empty(dvep));

  auto dvez = std::views::drop(e, 0);
  static_assert(is_optional<decltype(dvez)> == usesOptional);
  static_assert(std::is_same_v<decltype(*dvez.begin()), T&>);
  VERIFY(std::ranges::empty(dvez));
}

constexpr
bool
all_tests()
{
  test(42);
  int i = 42;
  int arr[10]{};
  NonMovable nm;
  NonAssignable na;
  test(&i);
  test(std::string_view("test"));
  test(std::vector<int>{1, 2, 3, 4});
  test(std::optional<int>(42));
  test<const int>(42);

  test<int&>(i);
  test<const int&>(i);
  test<int(&)[10]>(arr);
  test<const int(&)[10]>(arr);
  test<NonMovable&>(nm);
  test<const NonMovable&>(nm);
  test<NonAssignable&>(na);
  test<const NonAssignable&>(na);
  test_not_range<void(&)()>();
  test_not_range<void(&)(int)>();
  test_not_range<int(&)[]>();
  test_not_range<const int(&)[]>();

  range_chain_example();

  test_as_const<false, int>(i);
  test_as_const<false, const int>(i);
  test_as_const<true, int&>(i);
  test_as_const<true, const int&>(i);
  test_as_const<false, NonMovable, int>(10);
  test_as_const<false, const NonMovable, int>(10);
  test_as_const<true, NonMovable&>(nm);
  test_as_const<true, const NonMovable&>(nm);
  test_as_const<false, NonAssignable>({});
  test_as_const<false, const NonAssignable>({});
  test_as_const<true, NonAssignable&>(na);
  test_as_const<true, const NonAssignable&>(na);

#define TEST_ADAPTOR(name) \
  test_##name<true, int>(i); \
  test_##name<false, const int>(i); \
  test_##name<true, int&>(i); \
  test_##name<true, const int&>(i); \
  test_##name<false, NonMovable, int>(10); \
  test_##name<false, const NonMovable, int>(10); \
  test_##name<true, NonMovable&>(nm); \
  test_##name<true, const NonMovable&>(nm); \
  test_##name<false, NonAssignable>({}); \
  test_##name<false, const NonAssignable>({}); \
  test_##name<true, NonAssignable&>(na); \
  test_##name<true, const NonAssignable&>(na)

  TEST_ADAPTOR(reverse);
  TEST_ADAPTOR(take);
  TEST_ADAPTOR(drop);
#undef TEST_ADAPTOR
  return true;
}

static_assert(all_tests());

