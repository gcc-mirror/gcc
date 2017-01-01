// 2001-09-12 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test05()
{
  using namespace std;

  // Check money_get works with other iterators besides streambuf
  // input iterators.
  typedef string::const_iterator iter_type;
  typedef money_get<char, iter_type> mon_get_type;
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = goodbit;
  const locale loc_c = locale::classic();
  const string str = "1Eleanor Roosevelt";

  istringstream iss; 
  iss.imbue(locale(loc_c, new mon_get_type));

  // Iterator advanced, state, output.
  const mon_get_type& mg = use_facet<mon_get_type>(iss.getloc());

  // 01 string
  string res1;
  iter_type end1 = mg.get(str.begin(), str.end(), false, iss, err, res1);
  string rem1(end1, str.end());
  VERIFY( err == goodbit );
  VERIFY( res1 == "1" );
  VERIFY( rem1 == "Eleanor Roosevelt" );

  // 02 long double
  iss.clear();
  err = goodbit;
  long double res2;
  iter_type end2 = mg.get(str.begin(), str.end(), false, iss, err, res2);
  string rem2(end2, str.end());
  VERIFY( err == goodbit );
  VERIFY( res2 == 1 );
  VERIFY( rem2 == "Eleanor Roosevelt" );
}

int main()
{
  test05();
  return 0;
}
