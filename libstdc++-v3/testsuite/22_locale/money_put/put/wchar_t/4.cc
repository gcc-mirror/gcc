// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test04()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // Check money_put works with other iterators besides streambuf
  // output iterators. (As long as output_iterator requirements are met.)
  typedef wstring::iterator iter_type;
  typedef money_put<wchar_t, iter_type> mon_put_type;
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = goodbit;
  const locale loc_c = locale::classic();
  // woman, art, thief (stole the blues)
  const wstring str(L"1943 Janis Joplin");
  const long double ld = 1943.0;
  const wstring x(str.size(), 'x'); // have to have allocated wstring!
  wstring res;

  ostringstream oss; 
  oss.imbue(locale(loc_c, new mon_put_type));

  // Iterator advanced, state, output.
  const mon_put_type& mp = use_facet<mon_put_type>(oss.getloc());

  // 01 wstring
  res = x;
  iter_type ret1 = mp.put(res.begin(), false, oss, L' ', str);
  wstring sanity1(res.begin(), ret1);
  VERIFY( err == goodbit );
  VERIFY( res == L"1943xxxxxxxxxxxxx" );
  VERIFY( sanity1 == L"1943" );

  // 02 long double
  res = x;
  iter_type ret2 = mp.put(res.begin(), false, oss, L' ', ld);
  wstring sanity2(res.begin(), ret2);
  VERIFY( err == goodbit );
  VERIFY( res == L"1943xxxxxxxxxxxxx" );
  VERIFY( sanity2 == L"1943" );
}

int main()
{
  test04();
  return 0;
}
