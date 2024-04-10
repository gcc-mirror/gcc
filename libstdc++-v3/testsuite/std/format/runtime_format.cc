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

static_assert( noexcept(std::format_string<>(std::runtime_format(""))) );
static_assert( noexcept(std::wformat_string<>(std::runtime_format(L""))) );
static_assert( noexcept(std::format_string<int>(std::runtime_format(""))) );
static_assert( noexcept(std::wformat_string<char>(std::runtime_format(L""))) );
// A format string can be constructed from the result of std::runtime_format
// using copy elision, but cannot be constructed from an xvalue.
static_assert( !std::is_constructible_v<std::format_string<>,
					decltype(std::runtime_format(""))&&> );
static_assert( !std::is_constructible_v<std::wformat_string<>,
					decltype(std::runtime_format(L""))&&> );

int main()
{
  test_char();
  test_wchar();
  test_internal_api();
}
