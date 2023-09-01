// { dg-do run { target c++20 } }

#include <format>
#include <string>
#include <testsuite_hooks.h>

static_assert( std::is_base_of_v<std::runtime_error, std::format_error> );
static_assert( std::is_convertible_v<std::format_error*, std::runtime_error*> );

void
test_what()
{
  const char* cstr = "test string";
  std::format_error e(cstr);
  VERIFY( std::string(e.what()).find(cstr) != std::string::npos );

  std::string str = "test std::string";
  std::format_error ee(str);
  VERIFY( std::string(ee.what()).find(str) != std::string::npos );
}

int main()
{
  test_what();
}
