// { dg-do run { target c++26 } }

#include <span>
#include <stdexcept>
#include <testsuite_hooks.h>

void
test_at()
{
  int arr[4]{0, 1, 2, 3};
  std::span<int> s(arr);
  VERIFY(s.at(2) == 2);
#if __cpp_exceptions
  try {
    s.at(4); // { dg-warning "ignoring return value" "" { target exceptions_enabled } }
    VERIFY(false);
  } catch (const std::out_of_range&) {
  }
#endif

  auto s2 = s.subspan(1, 2);
  VERIFY(s2.at(0) == 1);
  VERIFY(s2.at(1) == 2);
#if __cpp_exceptions
  try {
    s2.at(2); // { dg-warning "ignoring return value" "" { target exceptions_enabled } }
    VERIFY(false);
  } catch (const std::out_of_range&) {
  }
#endif
}

int main()
{
  test_at();
}
