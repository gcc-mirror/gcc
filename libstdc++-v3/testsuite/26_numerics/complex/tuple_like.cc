// { dg-do compile { target c++26 } }

#include <complex>
#include <ranges>
#include <algorithm>
#include <string_view>
#include <type_traits>
#include <tuple>
#include <utility>

#include <testsuite_hooks.h>

template <typename T>
constexpr
void
test_sanity()
{
  using C = std::complex<T>;

  static_assert(std::tuple_size_v<C> == 2);
  static_assert(std::is_same_v<std::tuple_element_t<0, C>, T>);
  static_assert(std::is_same_v<std::tuple_element_t<1, C>, T>);

  static_assert(std::is_same_v<decltype(get<0>(std::declval<C&>())), T&>);
  static_assert(std::is_same_v<decltype(get<1>(std::declval<C&>())), T&>);
  static_assert(std::is_same_v<decltype(get<0>(std::declval<const C&>())), const T&>);
  static_assert(std::is_same_v<decltype(get<1>(std::declval<const C&>())), const T&>);
  static_assert(std::is_same_v<decltype(get<0>(std::declval<C>())), T&&>);
  static_assert(std::is_same_v<decltype(get<1>(std::declval<C>())), T&&>);
  static_assert(std::is_same_v<decltype(get<0>(std::declval<const C>())), const T&&>);
  static_assert(std::is_same_v<decltype(get<1>(std::declval<const C>())), const T&&>);
}

template <typename T>
constexpr
void
test_get()
{
  using C = std::complex<T>;

  C cpx(T(1), T(2));
  VERIFY(std::get<0>(cpx) == T(1));
  VERIFY(std::get<1>(cpx) == T(2));

  const C cpx2(T(3), T(4));
  VERIFY(std::get<0>(cpx2) == T(3));
  VERIFY(std::get<1>(cpx2) == T(4));

  struct derived : public C { using C::C; };
  derived cpx3(T(5), T(6));
  VERIFY(std::get<0>(cpx3) == T(5));
  VERIFY(std::get<1>(cpx3) == T(6));
}

template <typename T>
constexpr
void
test_structured_binding()
{
  using C = std::complex<T>;
  C cpx(T(1), T(2));

  auto& [r, i] = cpx;
  VERIFY(r == T(1));
  VERIFY(i == T(2));

  r = T(3);
  VERIFY(cpx.real() == T(3));
  VERIFY(cpx.imag() == T(2));

  i = T(4);
  VERIFY(cpx.real() == T(3));
  VERIFY(cpx.imag() == T(4));

  const C cpx2(T(5), T(6));
  auto& [r2, i2] = cpx2;
  VERIFY(r2 == T(5));
  VERIFY(i2 == T(6));
}

template <typename T>
constexpr
void
test_tuple_cat()
{
  std::complex<T> cpx(T(1), T(2));
  std::pair<int, std::string_view> p(42, "hello");

  auto r = std::tuple_cat(cpx, p, cpx);
  static_assert(std::is_same_v<decltype(r), std::tuple<T, T, int, std::string_view, T, T>>);
  VERIFY(std::get<0>(r) == T(1));
  VERIFY(std::get<1>(r) == T(2));
  VERIFY(std::get<2>(r) == 42);
  VERIFY(std::get<3>(r) == "hello");
  VERIFY(std::get<4>(r) == T(1));
  VERIFY(std::get<5>(r) == T(2));
}

template <typename T>
constexpr
void
test_element_view()
{
  std::complex<T> array[5] = {
    { T(0), T(1) },
    { T(2), T(3) },
    { T(4), T(5) },
    { T(6), T(7) },
    { T(8), T(9) }
  };

  T real_reduction = std::ranges::fold_left(array | std::views::keys, {}, std::plus{});
  VERIFY(real_reduction == T(20));

  T imag_reduction = std::ranges::fold_left(array | std::views::values, {}, std::plus{});
  VERIFY(imag_reduction == T(25));
}

template <typename T>
constexpr
void
test_apply()
{
  std::complex<T> cpx(T(1), T(2));

  auto f = [](T a, T b) { return a + b; };
  auto result = std::apply(f, cpx);

  VERIFY(result == T(3));
}

template <typename T>
constexpr
bool
all_tests()
{
  test_sanity<T>();
  test_structured_binding<T>();
  test_tuple_cat<T>();
  test_element_view<T>();
  test_apply<T>();
  test_get<T>();
  return true;
}

#define TEST(T) \
    static_assert(all_tests<T>()); \
    template T& std::get<0, T>(std::complex<T>&); \
    template T& std::get<1, T>(std::complex<T>&); \
    template T&& std::get<0, T>(std::complex<T>&&); \
    template T&& std::get<1, T>(std::complex<T>&&); \
    template const T& std::get<0, T>(const std::complex<T>&); \
    template const T& std::get<1, T>(const std::complex<T>&); \
    template const T&& std::get<0, T>(const std::complex<T>&&); \
    template const T&& std::get<1, T>(const std::complex<T>&&); \

TEST(float)
TEST(double)
TEST(long double)

#ifdef __STDCPP_FLOAT16_T__
TEST(_Float16)
#endif
#ifdef __STDCPP_FLOAT32_T__
TEST(_Float32)
#endif
#ifdef __STDCPP_FLOAT64_T__
TEST(_Float64)
#endif
#ifdef __STDCPP_FLOAT128_T__
TEST(_Float128)
#endif
#ifdef __STDCPP_BFLOAT16_T__
TEST(__gnu_cxx::__bfloat16_t)
#endif

TEST(char)
TEST(int)
TEST(unsigned int)
TEST(long)
