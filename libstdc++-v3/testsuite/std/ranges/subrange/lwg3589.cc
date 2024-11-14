// { dg-do compile { target c++20 } }

// LWG 3589. The const lvalue reference overload of get for subrange does not
// constrain I to be copyable when N == 0

#include <ranges>
#include <testsuite_iterators.h>

void
test_lwg3589()
{
  int a[2]{};
  __gnu_test::test_range<int, __gnu_test::input_iterator_wrapper_nocopy> r(a);

  // Use a generic lambda so we have a dependent context.
  auto test = [](auto& x)
    {
      // This was wrong before the LWG 3589 change:
      if constexpr (requires { std::ranges::get<0>(x); })
	(void) std::ranges::get<0>(x);

      // These always worked unconditionally:
      (void) std::ranges::get<1>(x);
      (void) std::ranges::get<0>(std::move(x));
      (void) std::ranges::get<1>(std::move(x));
    };

  std::ranges::subrange sr(r.begin(), r.end());
  test(sr);
}
