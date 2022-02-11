// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <string>
#include <testsuite_hooks.h>

constexpr bool
test_insert()
{
  std::string s;
  s.insert(0, "one");
  VERIFY( s == "one" );
  s.insert(0, "eleventy-");
  VERIFY( s == "eleventy-one" );
  s.insert(6, "ses at ten thirteen", 15);
  VERIFY( s == "elevenses at ten thirty-one" );

  return true;
}

static_assert( test_insert() );

constexpr bool
test_replace()
{
  std::string s = "abcdef";
  s.replace(2, 1, s.c_str(), 3);
  VERIFY( s == "ababcdef" );
  s.replace(0, 2, "", 0);
  VERIFY( s == "abcdef" );
  s.replace(1, 4, "ardwol", 6);
  VERIFY( s == "aardwolf" );
  s.replace(4, 0, "vark not wolf");

  return true;
}

static_assert( test_replace() );

constexpr bool
test_erasure()
{
  std::string s = "Spiritualized Electric Mainline";
  std::erase(s, 'i');
  VERIFY( s == "Sprtualzed Electrc Manlne" );
  std::erase_if(s, [](char c) { return c == 'l'; });
  VERIFY( s == "Sprtuazed Eectrc Manne" );

  return true;
}

static_assert( test_erasure() );
