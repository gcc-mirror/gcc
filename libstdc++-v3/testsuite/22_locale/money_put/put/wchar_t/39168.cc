// Copyright (C) 2009-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 22.2.6.2.1 money_put members

#include <sstream>
#include <locale>
#include <climits>
#include <testsuite_hooks.h>

class my_moneypunct: public std::moneypunct<wchar_t>
{
protected:
  std::string do_grouping() const { return std::string(1, CHAR_MAX); }
};

// libstdc++/39168
void test01()
{
  using namespace std;

  wostringstream oss;
  oss.imbue(locale(oss.getloc(), new my_moneypunct));
  const money_put<wchar_t>& mp = use_facet<money_put<wchar_t> >(oss.getloc());

  wstring digits(300, L'1');
  mp.put(oss.rdbuf(), false, oss, ' ', digits);
  VERIFY( oss.str() == digits );
}

int main()
{
  test01();
  return 0;
}
