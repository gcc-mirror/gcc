// { dg-do run { target c++11 } }

//
// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

// libstdc++/63199
void
test01()
{
  std::setlocale(LC_ALL, "");

  std::wstring current_token(L"II.");

  std::vector<std::wregex> regex_vector;

  for (int i = 0; i < 4; ++i)
  {
    std::regex_constants::syntax_option_type flag;
    flag = std::regex_constants::ECMAScript | std::regex_constants::icase;

    std::wregex reg;
    reg.imbue(std::locale(""));
    reg.assign(L"^(M*(?:CM|DC{1,3}|D|CD|C{1,3}){0,1}(?:XC|LX{1,3}|L|XL|X{1,3}){0,1}(?:IX|VI{0,3}|IV|I{1,3}){0,1}\\.)$", flag);

    regex_vector.emplace_back(reg);
  }

  for (auto cit = regex_vector.cbegin(); cit != regex_vector.cend(); ++cit)
  {
    std::wstring::const_iterator it1 = current_token.begin();
    std::wstring::const_iterator it2 = current_token.end();
    std::wsmatch current_token_match;

    regex_match_debug(it1, it2, current_token_match, *cit);
    VERIFY(current_token_match[0] == current_token);
    VERIFY(current_token_match[1] == current_token);
  }
}

int
main()
{
  test01();
  return 0;
}
