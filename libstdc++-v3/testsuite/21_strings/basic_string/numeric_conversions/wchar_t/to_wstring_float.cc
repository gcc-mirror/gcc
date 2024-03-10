// { dg-do run { target c++11 } }
// { dg-require-namedlocale "de_DE.ISO8859-15" }
// { dg-require-string-conversions "" }

// C++11 21.5 Numeric Conversions [string.conversions]

#include <string>
#include <format>
#include <limits>
#include <locale>
#include <cstdio>
#include <testsuite_hooks.h>

namespace test
{
// Canonical version of std::to_wstring(double) as specified in the standard.

#if __cplusplus > 202302L

std::wstring to_wstring(float val) { return std::format(L"{}", val); }
std::wstring to_wstring(double val) { return std::format(L"{}", val); }
std::wstring to_wstring(long double val) { return std::format(L"{}", val); }

#else

std::wstring to_wstring(double val)
{
  std::wstring str(100, L'9');
retry:
  const int size = str.size();
  const int len = std::swprintf(&str[0], size + 1, L"%f", val);
  if (len == -1) // N.B. swprintf just returns -1 if the buffer is too small.
  {
    str.resize(size * 2);
    goto retry;
  }
  str.resize(len);
  return str;
}

// snprintf promotes float to double
std::wstring to_wstring(float val) { return to_wstring((double)val); }

std::wstring to_wstring(long double val)
{
  std::wstring str(100, L'9');
retry:
  const int size = str.size();
  const int len = std::swprintf(&str[0], size + 1, L"%Lf", val);
  if (len == -1) // N.B. swprintf just returns -1 if the buffer is too small.
  {
    str.resize(size * 2);
    goto retry;
  }
  str.resize(len);
  return str;
}
#endif
} // namespace test

template<typename T>
  void check_value(T val)
  {
    const std::wstring s = std::to_wstring(val);
    const std::wstring expected = test::to_wstring(val);
    VERIFY( s == expected );
    VERIFY( s[s.size()] == L'\0' ); // null-terminator not overwritten
  }

template<typename T>
  void check_values()
  {
    const T values[] = {
      0.0, 0.0625, 0.25, 0.5, 1.25, 1e2, 1e7, 1e8, 1e-2, 1e-7, 1e-8,
      2e38, 4.4e+19, 6.25e-12, 7.89e+23,
      12345.6789, (T) 1234567890123456.e100L, (T) 1213141516e-99L,
      std::numeric_limits<T>::min(),
      std::numeric_limits<T>::max(),
      std::numeric_limits<T>::epsilon(),
      std::numeric_limits<T>::infinity(),
      std::numeric_limits<T>::quiet_NaN(),
    };

    std::locale::global(std::locale::classic());

    for (auto v : values)
    {
      check_value(v);
      check_value(-v);
    }

    std::locale::global(std::locale(ISO_8859(15,de_DE)));

    for (auto v : values)
    {
      check_value(v);
      check_value(-v);
    }

    std::locale::global(std::locale::classic());
  }

void test01()
{
  // Examples from P2587R3 `to_string` or not `to_string`


  VERIFY( std::to_wstring(42) == L"42" );
  VERIFY( std::to_wstring(12345) == L"12345" );
  auto max = std::to_wstring(1.7976931348623157e+308);

#if __cplusplus <= 202302L
  VERIFY( std::to_wstring(0.42) == L"0.420000" );
  VERIFY( std::to_wstring(1e-7) == L"0.000000" );
  VERIFY( std::to_wstring(-1e-7) == L"-0.000000" );
  VERIFY( max.substr(0, 17) == L"17976931348623157" );
  VERIFY( max.substr(max.size() - 7) == L".000000" );
#else
  VERIFY( std::to_wstring(0.42) == L"0.42" );
  VERIFY( std::to_wstring(1e-7) == L"1e-07" );
  VERIFY( std::to_wstring(-1e-7) == L"-1e-07" );
  VERIFY( max == L"1.7976931348623157e+308" );
#endif

  std::locale::global(std::locale(ISO_8859(15,de_DE)));
#if __cplusplus <= 202302L
  VERIFY( std::to_wstring(1234.5) == L"1234,500000" );
#else
  VERIFY( std::to_wstring(1234.5) == L"1234.5" );
#endif
  std::locale::global(std::locale::classic());
}

void test02()
{
  check_values<float>();
  check_values<double>();
  check_values<long double>();
}

int main()
{
  test01();
  test02();
}
