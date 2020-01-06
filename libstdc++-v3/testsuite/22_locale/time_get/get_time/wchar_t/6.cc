// 2010-10-07  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  typedef wstring::const_iterator iter_type;
  typedef time_get<wchar_t, iter_type> time_get_type;
  const ios_base::iostate goodbit = ios_base::goodbit;
  const ios_base::iostate eofbit = ios_base::eofbit;  
  const ios_base::iostate failbit = ios_base::failbit;
  ios_base::iostate err = goodbit;
  const locale loc_c = locale::classic();

  // Create "C" time objects
  tm tm0 = __gnu_test::test_tm(0, 0, 0, 0, 0, 0, 0, 0, 0);
  tm tm1 = __gnu_test::test_tm(0, 0, 0, 0, 0, 0, 0, 0, 0);

  wistringstream iss;
  iss.imbue(locale(loc_c, new time_get_type));

  // Iterator advanced, state, output.
  const time_get_type& tg = use_facet<time_get_type>(iss.getloc());

  const wstring str0 = L"12";
  tg.get_time(str0.begin(), str0.end(), iss, err, &tm0);
  VERIFY( err == (failbit | eofbit) );
  VERIFY( tm0.tm_sec == 0 );
  VERIFY( tm0.tm_min == 0 );
  // This is quite hard to guarantee now, revisit together with DR 461
  // in the C++0x context.
  // VERIFY( tm0.tm_hour == 0 );

  const wstring str1 = L"12:30 ";
  err = goodbit;
  iter_type end1 = tg.get_time(str1.begin(), str1.end(), iss, err, &tm1);
  VERIFY( err == failbit );
  VERIFY( *end1 == ' ' );
  VERIFY( tm1.tm_sec == 0 );
  // See above...
  // VERIFY( tm1.tm_min == 0 );
  // VERIFY( tm1.tm_hour == 0 );
}

int main()
{
  test01();
  return 0;
}
