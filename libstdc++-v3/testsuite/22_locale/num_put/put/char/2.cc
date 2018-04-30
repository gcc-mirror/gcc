// 2001-11-19 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2018 Free Software Foundation, Inc.
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

void test02()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc_c = locale::classic();

  // sanity check the data is correct.
  const string empty;
  string result1;
  string result2;

  bool b1 = true;
  bool b0 = false;
  unsigned long ul1 = 1294967294;
  unsigned long ul2 = 0;

  // cache the num_put facet
  ostringstream oss;
  oss.imbue(loc_c);
  const num_put<char>& np = use_facet<num_put<char> >(oss.getloc()); 

  // C
  // bool, more twisted examples
  oss.str(empty);
  oss.width(20);
  oss.setf(ios_base::right, ios_base::adjustfield);
  np.put(oss.rdbuf(), oss, '+', b0);
  result1 = oss.str();
  VERIFY( result1 == "+++++++++++++++++++0" );

  oss.str(empty);
  oss.width(20);
  oss.setf(ios_base::left, ios_base::adjustfield);
  oss.setf(ios_base::boolalpha);
  np.put(oss.rdbuf(), oss, '+', b1);
  result2 = oss.str();
  VERIFY( result2 == "true++++++++++++++++" );

  // unsigned long, in a locale that does not group
  oss.imbue(loc_c);
  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, '+', ul1);
  result1 = oss.str();
  VERIFY( result1 == "1294967294" );

  oss.str(empty);
  oss.clear();
  oss.width(20);
  oss.setf(ios_base::left, ios_base::adjustfield);
  np.put(oss.rdbuf(), oss, '+', ul2);
  result1 = oss.str();
  VERIFY( result1 == "0+++++++++++++++++++" );
}

int main()
{
  test02();
  return 0;
}


