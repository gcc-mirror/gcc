// PR libstdc++/103955
// Verify we don't crash when the floating-point to_chars overloads are passed
// a large precision argument.
// { dg-do run { target c++17 } }

#include <charconv>

#include <climits>
#include <initializer_list>
#include <testsuite_hooks.h>

void
test01()
{
  const int size = 12;
  char result[size];

  for (auto fmt : { std::chars_format::fixed, std::chars_format::scientific,
		    std::chars_format::general, std::chars_format::hex })
    for (int precision : { INT_MAX, INT_MAX-1, INT_MAX-2 })
      {
	auto tcr = std::to_chars(result, result+size, 1.337, fmt, precision);
	VERIFY( tcr.ptr == result+size && tcr.ec == std::errc::value_too_large );
      }
}

int
main()
{
  test01();
}
