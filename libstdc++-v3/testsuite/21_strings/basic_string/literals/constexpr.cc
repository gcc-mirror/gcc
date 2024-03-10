// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <string>
#include <testsuite_hooks.h>

constexpr bool
test_literals()
{
  using namespace std::literals;

  auto s = "narrow string"s;
  auto sw = L"wide string"s;
  auto s8 = u8"UTF-8 string"s;
  auto su = u"UTF-16 string"s;
  auto sU = U"UTF-32 string"s;

  return !s.empty() && !sw.empty() && !s8.empty() && !su.empty() && !sU.empty();
}

static_assert( test_literals() );
