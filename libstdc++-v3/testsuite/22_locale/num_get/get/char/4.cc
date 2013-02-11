// 2001-11-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2013 Free Software Foundation, Inc.
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

// 22.2.2.1.1  num_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// 2002-01-10  David Seymour  <seymour_dj@yahoo.com>
// libstdc++/5331
void test04()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // Check num_get works with other iterators besides streambuf
  // output iterators. (As long as output_iterator requirements are met.)
  typedef string::const_iterator iter_type;
  typedef num_get<char, iter_type> num_get_type;
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = ios_base::goodbit;
  const locale loc_c = locale::classic();
  const string str("20000106 Elizabeth Durack");
  const string str2("0 true 0xbffff74c Durack");

  istringstream iss; // need an ios, add my num_get facet
  iss.imbue(locale(loc_c, new num_get_type));

  // Iterator advanced, state, output.
  const num_get_type& ng = use_facet<num_get_type>(iss.getloc());

  // 01 get(long)
  // 02 get(long double)
  // 03 get(bool)
  // 04 get(void*)

  // 01 get(long)
  long i = 0;
  err = goodbit;
  iter_type end1 = ng.get(str.begin(), str.end(), iss, err, i);
  string rem1(end1, str.end());
  VERIFY( err == goodbit );
  VERIFY( i == 20000106);
  VERIFY( rem1 == " Elizabeth Durack" );

  // 02 get(long double)
  long double ld = 0.0;
  err = goodbit;
  iter_type end2 = ng.get(str.begin(), str.end(), iss, err, ld);
  string rem2(end2, str.end());
  VERIFY( err == goodbit );
  VERIFY( ld == 20000106);
  VERIFY( rem2 == " Elizabeth Durack" );

  // 03 get(bool)
  bool b = 1;
  iss.clear();
  err = goodbit;
  iter_type end3 = ng.get(str2.begin(), str2.end(), iss, err, b);
  string rem3(end3, str2.end());
  VERIFY( err == goodbit );
  VERIFY( b == 0 );
  VERIFY( rem3 == " true 0xbffff74c Durack" );

  iss.clear();
  err = goodbit;
  iss.setf(ios_base::boolalpha);
  iter_type end4 = ng.get(++end3, str2.end(), iss, err, b);
  string rem4(end4, str2.end());
  VERIFY( err == goodbit );
  VERIFY( b == true );
  VERIFY( rem4 == " 0xbffff74c Durack" );

  // 04 get(void*)
  void* v;
  iss.clear();
  err = goodbit;
  iss.setf(ios_base::fixed, ios_base::floatfield);
  iter_type end5 = ng.get(++end4, str2.end(), iss, err, v);
  string rem5(end5, str2.end());
  VERIFY( err == goodbit );
  VERIFY( b == true );
  VERIFY( rem5 == " Durack" );
}

int main()
{
  test04();
  return 0;
}


// Kathleen Hannah, humanitarian, woman, art-thief
