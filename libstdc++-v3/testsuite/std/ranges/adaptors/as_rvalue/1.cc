// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

#if __cpp_lib_ranges_as_rvalue != 202207L
# error "Feature-test macro __cpp_lib_ranges_as_rvalue has wrong value in <ranges>"
#endif

namespace ranges = std::ranges;
namespace views = std::views;


/* Replacement for the standard version, as it's not available in freestanding
   environments.  */
template<typename T, typename... Args>
requires (!std::is_array_v<T>)
constexpr auto
make_unique (Args &&...args)
{
  return std::unique_ptr<T> { new T (std::forward<Args>(args)...) };
}

constexpr bool
test01()
{

  std::unique_ptr<int> a[3] = { make_unique<int>(1),
				make_unique<int>(2),
				make_unique<int>(3) };
  std::unique_ptr<int> b[3];
  auto v = a | views::as_rvalue;
  ranges::copy(v, b);
  VERIFY( ranges::all_of(a, [](auto& p) { return p.get() == nullptr; }) );
  VERIFY( ranges::equal(b | views::transform([](auto& p) { return *p; }), (int[]){1, 2, 3}) );

  return true;
}

void
test02()
{
  std::unique_ptr<int> x = make_unique<int>(42);
  std::unique_ptr<int> y;
  __gnu_test::test_input_range rx(&x, &x+1);
  auto v = rx | views::as_rvalue;
  static_assert(!ranges::common_range<decltype(v)>);
  ranges::copy(v, &y);
  VERIFY( x.get() == nullptr );
  VERIFY( *y == 42 );
}

int
main()
{
  static_assert(test01());
  test02();
}
