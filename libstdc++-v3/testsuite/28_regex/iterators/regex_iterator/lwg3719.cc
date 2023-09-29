// { dg-do run { target c++20 } }

#include <regex>
#include <iterator>
#include <testsuite_hooks.h>

// LWG 3719. Directory iterators should be usable with default sentinel

void
test_iter()
{
  std::sregex_token_iterator r0;
  VERIFY( r0 == std::default_sentinel );
  std::string haystack = "a needle in a haystack";
  std::regex needle("needle");
  std::sregex_iterator r1(haystack.begin(), haystack.end(), needle);
  VERIFY( r1 != std::default_sentinel );
  ++r1;
  VERIFY( r1 == std::default_sentinel );

  static_assert( noexcept(r0 == std::default_sentinel) ); // GCC extension
  static_assert( noexcept(r0 != std::default_sentinel) ); // GCC extension
}

int main()
{
  test_iter();
}
