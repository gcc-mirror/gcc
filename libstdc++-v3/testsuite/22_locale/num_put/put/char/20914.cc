// { dg-require-namedlocale "de_DE" }

// 2005-04-17  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2013 Free Software Foundation, Inc.
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

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/20914
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // A locale that expects grouping.
  locale loc_de = locale("de_DE");

  const string empty;
  string result;

  ostringstream oss;
  oss.imbue(loc_de);
  const num_put<char>& np = use_facet<num_put<char> >(oss.getloc()); 

  long l0 = -300000;
  long l1 = 300;
  double d0 = -300000;
  double d1 = 300;

  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, '*', l0);
  result = oss.str();
  VERIFY( result == "-300.000" );

  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, '*', d0);
  result = oss.str();
  VERIFY( result == "-300.000" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios::showpos);
  np.put(oss.rdbuf(), oss, '*', l1);
  result = oss.str();
  VERIFY( result == "+300" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios::showpos);
  np.put(oss.rdbuf(), oss, '*', d1);
  result = oss.str();
  VERIFY( result == "+300" );
}

int main()
{
  test01();
  return 0;
}
