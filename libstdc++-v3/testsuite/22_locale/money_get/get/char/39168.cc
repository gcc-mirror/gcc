// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

// 22.2.6.1.1 money_get members

#include <sstream>
#include <locale>
#include <climits>
#include <testsuite_hooks.h>

class my_moneypunct: public std::moneypunct<char>
{
protected:
  std::string do_grouping() const { return std::string(1, CHAR_MAX); }
};

// libstdc++/39168
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;

  istringstream iss;
  iss.imbue(locale(iss.getloc(), new my_moneypunct));
  const money_get<char>& mg = use_facet<money_get<char> >(iss.getloc());

  string digits;
  ios_base::iostate err = ios_base::goodbit;

  iss.str("123,456");
  iterator_type end = mg.get(iss.rdbuf(), 0, false, iss, err, digits);
  VERIFY( err == ios_base::goodbit );
  VERIFY( digits == "123" );
  VERIFY( *end == ',' );
}

int main()
{
  test01();
  return 0;
}
