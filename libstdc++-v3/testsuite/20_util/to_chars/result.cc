// { dg-do run { target c++26 } }
// { dg-add-options no_pch }

#include <charconv>

#if defined(__cpp_lib_to_chars) && __cpp_lib_to_chars < 202306L
# error "Feature-test macro for std::to_chars has wrong value in <charconv>"
#endif

#include <testsuite_hooks.h>

void
test_result()
{
  static_assert( ! std::is_convertible_v<std::to_chars_result, bool> );
  static_assert( std::is_constructible_v<bool, std::to_chars_result> );

  std::to_chars_result res{};
  VERIFY( res );
  res.ec = std::errc::invalid_argument;
  VERIFY( !res );
}

int main()
{
  test_result();
}
