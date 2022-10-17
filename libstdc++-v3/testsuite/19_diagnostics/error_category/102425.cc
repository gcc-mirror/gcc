// { dg-do run { target c++11 } }
#include <system_error>
#include <testsuite_hooks.h>

void test01()
{
  // PR libstdc++/102425
  VERIFY( std::error_code() == std::error_condition() );

  auto zero = std::system_category().default_error_condition(0);
  // This is the condition that the equality above relies on:
  VERIFY( zero.category() == std::generic_category() );
}

int main()
{
  test01();
}
