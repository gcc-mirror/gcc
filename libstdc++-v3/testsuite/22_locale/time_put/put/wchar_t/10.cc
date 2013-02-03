// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.5.3.1 time_put members

// { dg-do run { xfail dummy_wcsftime } }

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test10()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // Check time_put works with other iterators besides streambuf
  // output iterators. (As long as output_iterator requirements are met.)
  typedef wstring::iterator iter_type;
  typedef char_traits<wchar_t> traits;
  typedef time_put<wchar_t, iter_type> time_put_type;
  const ios_base::iostate goodbit = ios_base::goodbit;

  ios_base::iostate err = goodbit;
  const locale loc_c = locale::classic();
  const wstring x(50, 'x'); // have to have allocated wstring!
  wstring res;
  const tm time_sanity = __gnu_test::test_tm(0, 0, 12, 26, 5, 97, 2, 0, 0);

  wostringstream oss; 
  oss.imbue(locale(loc_c, new time_put_type));
  const time_put_type& tp = use_facet<time_put_type>(oss.getloc());

  // 02 char format
  res = x;
  iter_type ret2 = tp.put(res.begin(), oss, L' ', &time_sanity, 'A');
  wstring sanity2(res.begin(), ret2);
  VERIFY( err == goodbit );
  VERIFY( res == L"Tuesdayxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" );
  VERIFY( sanity2 == L"Tuesday" );
}

int main()
{
  test10();
  return 0;
}
