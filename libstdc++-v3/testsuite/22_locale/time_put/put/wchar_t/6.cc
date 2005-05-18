// { dg-require-namedlocale "" }

// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test06()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> iterator_type;
  typedef char_traits<wchar_t> traits;

  bool test __attribute__((unused)) = true;

  // create "C" time objects
  tm time1 = { 0, 0, 12, 4, 3, 71, 0, 93, 0 };
  const wchar_t* date = L"%A, the second of %B";
  const wchar_t* date_ex = L"%Ex";

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_de = locale("de_DE");
  VERIFY( loc_de != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const wstring empty;
  wostringstream oss;
  oss.imbue(loc_de);
  const time_put<wchar_t>& tim_put = use_facet<time_put<wchar_t> >(oss.getloc()); 

  iterator_type os_it07 = tim_put.put(oss.rdbuf(), oss, L'*', &time1, 
				      date, date + traits::length(date));
  wstring result7 = oss.str();
  VERIFY( result7 == L"Sonntag, the second of April");
  iterator_type os_it08 = tim_put.put(oss.rdbuf(), oss, L'*', &time1, 
				      date_ex, date_ex + traits::length(date));
  wstring result8 = oss.str();
  VERIFY( result8 != result7 );
}

int main()
{
  test06();
  return 0;
}
