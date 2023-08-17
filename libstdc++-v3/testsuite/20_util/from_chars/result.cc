// { dg-do run { target c++26 } }

#include <charconv>
#include <testsuite_hooks.h>

void
test_result()
{
  static_assert( ! std::is_convertible_v<std::from_chars_result, bool> );
  static_assert( std::is_constructible_v<bool, std::from_chars_result> );

  std::from_chars_result res{};
  VERIFY( res );
  res.ec = std::errc::invalid_argument;
  VERIFY( !res );
}

int main()
{
  test_result();
}
