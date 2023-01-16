// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }
//
// 2009-06-17  Stephen M. Webb  <stephen.webb@xandros.com>
//
// Copyright (C) 2009-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// C++11 28.5.1 [re.synopt]

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_common_types.h>

void
test01()
{
  std::regex_constants::syntax_option_type option { };
  option = option | std::regex_constants::icase;
  option = option | std::regex_constants::nosubs;
  option = option | std::regex_constants::optimize;
  option = option | std::regex_constants::collate;
  option = option | std::regex_constants::ECMAScript;
  option = option | std::regex_constants::basic;
  option = option | std::regex_constants::extended;
  option = option | std::regex_constants::awk;
  option = option | std::regex_constants::grep;
  option = option | std::regex_constants::egrep;
}

void
test02()
{
  std::regex_constants::syntax_option_type option { };
  option = option & std::regex_constants::icase;
  option = option & std::regex_constants::nosubs;
  option = option & std::regex_constants::optimize;
  option = option & std::regex_constants::collate;
  option = option & std::regex_constants::ECMAScript;
  option = option & std::regex_constants::basic;
  option = option & std::regex_constants::extended;
  option = option & std::regex_constants::awk;
  option = option & std::regex_constants::grep;
  option = option & std::regex_constants::egrep;
}

void
test03()
{
  std::regex_constants::syntax_option_type option { };
  option = ~std::regex_constants::icase;
  option = ~std::regex_constants::nosubs;
  option = ~std::regex_constants::optimize;
  option = ~std::regex_constants::collate;
  option = ~std::regex_constants::ECMAScript;
  option = ~std::regex_constants::basic;
  option = ~std::regex_constants::extended;
  option = ~std::regex_constants::awk;
  option = ~std::regex_constants::grep;
  option = ~std::regex_constants::egrep;
  option = option;
}

void
test04_constexpr()
{
  using namespace std::regex_constants;
  constexpr auto a1 __attribute__((unused)) = icase | awk;
  constexpr auto a2 __attribute__((unused)) = icase & awk;
  constexpr auto a3 __attribute__((unused)) = ~grep;
}

void
test05()
{
  using namespace std;
  using namespace regex_constants;
  regex re("((a)(s))", nosubs | ECMAScript);
  VERIFY(re.mark_count() == 0);
}

#if __cplusplus >= 201402L
static_assert(
    __gnu_test::test_bitmask_values( {
      std::regex_constants::icase,
      std::regex_constants::nosubs,
      std::regex_constants::optimize,
      std::regex_constants::collate,
      std::regex_constants::ECMAScript,
      std::regex_constants::basic,
      std::regex_constants::extended,
      std::regex_constants::awk,
      std::regex_constants::grep,
      std::regex_constants::egrep,
      std::regex_constants::__multiline,
      std::regex_constants::__polynomial
    }, {
#if __cplusplus >= 201703L
      std::regex_constants::multiline // equal to __multiline
#endif
    }),
    "std::regex_constants::syntax_option_type bitmask elements are distinct" );
#endif

int main()
{
  test01();
  test02();
  test03();
  test04_constexpr();
  test05();
  return 0;
}
