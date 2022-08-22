// { dg-do run { target c++11 } }

#include <regex>
#include <string>
#include <climits>
#include <testsuite_hooks.h>

// PR libstdc++/106607 - Regex integer overflow on large backreference value

int main()
{
  std::regex r("(.)\\1"); // OK

  try
  {
    long long n = (unsigned)-1 + 2LL;          // 4294967297 for 32-bit int
    VERIFY( (int)n == 1 );                     // 4294967297 % 2^32 == 1
    std::regex r("(.)\\" + std::to_string(n)); // Invalid back reference.
    VERIFY(false);
  }
  catch (const std::regex_error& e)
  {
    VERIFY( e.code() == std::regex_constants::error_backref );
  }
}
