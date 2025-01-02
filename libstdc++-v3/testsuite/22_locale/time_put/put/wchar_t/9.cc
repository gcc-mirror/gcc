// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

void test09()
{
  using namespace std;

  // Check time_put works with other iterators besides streambuf
  // output iterators. (As long as output_iterator requirements are met.)
  typedef wstring::iterator iter_type;
  typedef char_traits<wchar_t> traits;
  typedef time_put<wchar_t, iter_type> time_put_type;
  const ios_base::iostate goodbit = ios_base::goodbit;

  ios_base::iostate err = goodbit;
  const locale loc_c = locale::classic();
  const wstring x(50, L'x'); // have to have allocated wstring!
  wstring res;
  const tm time_sanity = __gnu_test::test_tm(0, 0, 12, 26, 5, 97, 2, 0, 0);
  const wchar_t* date = L"%X, %A, the second of %B, %Y";

  wostringstream oss; 
  oss.imbue(locale(loc_c, new time_put_type));
  const time_put_type& tp = use_facet<time_put_type>(oss.getloc());

  // 01 date format
  res = x;
  iter_type ret1 = tp.put(res.begin(), oss, L' ', &time_sanity, 
			  date, date + traits::length(date));
  wstring sanity1(res.begin(), ret1);
  VERIFY( err == goodbit );
  VERIFY( res == L"12:00:00, Tuesday, the second of June, 1997xxxxxxx" );
  VERIFY( sanity1 == L"12:00:00, Tuesday, the second of June, 1997" );
}

int main()
{
  test09();
  return 0;
}
