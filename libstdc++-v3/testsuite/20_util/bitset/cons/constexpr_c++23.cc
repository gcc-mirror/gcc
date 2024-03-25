// { dg-do compile { target c++23 } }
// This test relies on std::string.
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <bitset>

#ifndef __cpp_lib_constexpr_bitset
# error "Feature-test macro for constexpr bitset missing in <bitset>"
#elif __cpp_lib_constexpr_bitset != 202202L
# error "Feature-test macro for constexpr bitset has wrong value in <bitset>"
#endif

#include <testsuite_hooks.h>

constexpr bool test_ntbs()
{
  VERIFY( std::bitset<0>("000").all() );
  VERIFY( std::bitset<0>("000", 2).all() );
  VERIFY( std::bitset<1>("100", 2).all() );
  VERIFY( std::bitset<1>("z00", 2, 'z').none() );
  VERIFY( std::bitset<2>("ab0", 3, 'a', 'b').count() == 1 );

  return true;
}

static_assert( test_ntbs() );

#if _GLIBCXX_USE_CXX11_ABI
constexpr bool test_string()
{
  using S = std::string;
  VERIFY( std::bitset<0>(S("000")).all() );
  VERIFY( std::bitset<1>(S("010"), 1, 2).all() );
  VERIFY( std::bitset<2>(S("0110"), 1, 2).all() );
  VERIFY( std::bitset<2>(S("1z110"), 1, 3, 'z').count() == 1 );
  VERIFY( std::bitset<3>(S("0abab0"), 2, 3, 'a', 'b').count() == 2 );

  return true;
}

static_assert( test_string() );

constexpr bool test_wstring()
{
  using S = std::wstring;
  VERIFY( std::bitset<0>(S(L"000")).all() );
  VERIFY( std::bitset<1>(S(L"010"), 1, 2).all() );
  VERIFY( std::bitset<2>(S(L"0110"), 1, 2).all() );
  VERIFY( std::bitset<2>(S(L"1z110"), 1, 3, L'z').count() == 1 );
  VERIFY( std::bitset<3>(S(L"0abab0"), 2, 3, L'a', L'b').count() == 2 );

  return true;
}

static_assert( test_wstring() );
#endif
