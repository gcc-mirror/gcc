// { dg-do run { target c++26 } }

#include <format>
#include <testsuite_hooks.h>

void
test_char()
{
  std::string fmt = "{}";
  auto s = std::format(std::runtime_format(fmt), 123);
  VERIFY( s == "123" );
}

void
test_wchar()
{
  std::wstring fmt = L"{:#o}";
  auto s = std::format(std::runtime_format(fmt), 456);
  VERIFY( s == L"0710" );
}

void
test_internal_api()
{
  // Using _Runtime_format_string directly works even in C++20 mode.
  // This can be used internally by libstdc++.
  std::string fmt = "{:#x}";
  auto s = std::format(std::__format::_Runtime_format_string<char>(fmt), 789);
  VERIFY( s == "0x315" );
}

int main()
{
  test_char();
  test_wchar();
  test_internal_api();
}
