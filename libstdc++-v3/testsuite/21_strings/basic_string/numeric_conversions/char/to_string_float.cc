// { dg-do run { target c++11 } }
// { dg-require-namedlocale "de_DE.ISO8859-15" }
// { dg-add-options no_pch }

// C++11 21.5 Numeric Conversions [string.conversions]

#include <string>

#if __cplusplus > 202302L

#ifndef __cpp_lib_to_string
# error "Feature-test macro for std::to_string missing in <string>"
#elif __cpp_lib_to_string != 202306L
# error "Feature-test macro for std::to_string has wrong value in <string>"
#endif

#else

#ifdef __cpp_lib_to_string
# error "__cpp_lib_to_string should not be defined for C++23"
#endif

#endif

#include <format>
#include <limits>
#include <locale>
#include <cstdio>
#include <testsuite_hooks.h>

namespace test
{
// Canonical version of std::to_string(double) as specified in the standard.

#if __cpp_lib_to_string
static std::string to_string(float val) { return std::format("{}", val); }
static std::string to_string(double val) { return std::format("{}", val); }
static std::string to_string(long double val) { return std::format("{}", val); }
#else
static std::string to_string(double val)
{
  std::string str(100, '9');
retry:
  const int size = str.size();
  const int len = std::snprintf(&str[0], size + 1, "%f", val);
  str.resize(len);
  if (len > size)
    goto retry;
  return str;
}

// snprintf promotes float to double
static std::string to_string(float val) { return to_string((double)val); }

static std::string to_string(long double val)
{
  std::string str(100, '9');
retry:
  const int size = str.size();
  const int len = std::snprintf(&str[0], size + 1, "%Lf", val);
  str.resize(len);
  if (len > size)
    goto retry;
  return str;
}
#endif
} // namespace test

template<typename T>
  void check_value(T val)
  {
    const std::string s = std::to_string(val);
    const std::string expected = test::to_string(val);
    VERIFY( s == expected );
    VERIFY( s[s.size()] == '\0' ); // null-terminator not overwritten
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


  VERIFY( std::to_string(42) == "42" );
  VERIFY( std::to_string(12345) == "12345" );
  auto max = std::to_string(1.7976931348623157e+308);

#if __cplusplus <= 202302L
  VERIFY( std::to_string(0.42) == "0.420000" );
  VERIFY( std::to_string(1e-7) == "0.000000" );
  VERIFY( std::to_string(-1e-7) == "-0.000000" );
  VERIFY( max.substr(0, 17) == "17976931348623157" );
  VERIFY( max.substr(max.size() - 7) == ".000000" );
#else
  VERIFY( std::to_string(0.42) == "0.42" );
  VERIFY( std::to_string(1e-7) == "1e-07" );
  VERIFY( std::to_string(-1e-7) == "-1e-07" );
  VERIFY( max == "1.7976931348623157e+308" );
#endif

  std::locale::global(std::locale(ISO_8859(15,de_DE)));
#if __cplusplus <= 202302L
  VERIFY( std::to_string(1234.5) == "1234,500000" );
#else
  VERIFY( std::to_string(1234.5) == "1234.5" );
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
