// 2001-11-19 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

void test04()
{
  using namespace std;

  // Check num_put works with other iterators besides streambuf
  // output iterators. (As long as output_iterator requirements are met.)
  typedef string::iterator iter_type;
  typedef char_traits<char> traits;
  typedef num_put<char, iter_type> num_put_type;
  const locale loc_c = locale::classic();
  const string str("1798 Lady Elgin");
  const string x(18, 'x'); // have to have allocated string!
                           // allow for "0x" + 16 hex digits (64-bit pointer)
  string res;

  ostringstream oss; 
  oss.imbue(locale(loc_c, new num_put_type));

  // Iterator advanced, state, output.
  const num_put_type& tp = use_facet<num_put_type>(oss.getloc());

  // 01 put(long)
  // 02 put(long double)
  // 03 put(bool)
  // 04 put(void*)

  // 01 put(long)
  const long l = 1798;
  res = x;
  iter_type ret1 = tp.put(res.begin(), oss, ' ', l);
  string sanity1(res.begin(), ret1);
  VERIFY( res == "1798xxxxxxxxxxxxxx" );
  VERIFY( sanity1 == "1798" );

  // 02 put(long double)
  const long double ld = 1798.0;
  res = x;
  iter_type ret2 = tp.put(res.begin(), oss, ' ', ld);
  string sanity2(res.begin(), ret2);
  VERIFY( res == "1798xxxxxxxxxxxxxx" );
  VERIFY( sanity2 == "1798" );

  // 03 put(bool)
  bool b = 1;
  res = x;
  iter_type ret3 = tp.put(res.begin(), oss, ' ', b);
  string sanity3(res.begin(), ret3);
  VERIFY( res == "1xxxxxxxxxxxxxxxxx" );
  VERIFY( sanity3 == "1" );

  b = 0;
  res = x;
  oss.setf(ios_base::boolalpha);
  iter_type ret4 = tp.put(res.begin(), oss, ' ', b);
  string sanity4(res.begin(), ret4);
  VERIFY( res == "falsexxxxxxxxxxxxx" );
  VERIFY( sanity4 == "false" );

  // 04 put(void*)
  oss.clear();
  const void* cv = &ld;
  res = x;
  oss.setf(ios_base::fixed, ios_base::floatfield);
  iter_type ret5 = tp.put(res.begin(), oss, ' ', cv);
  string sanity5(res.begin(), ret5);
  VERIFY( sanity5.size() );
  VERIFY( sanity5[1] == 'x' );
}

int main()
{
  test04();
  return 0;
}


