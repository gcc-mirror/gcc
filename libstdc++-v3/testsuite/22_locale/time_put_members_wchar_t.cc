// 2001-10-02 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001 Free Software Foundation
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

// XXX This test is not working for non-glibc locale models.
// { dg-do run { xfail *-*-* } }

#ifdef _GLIBCPP_USE_WCHAR_T
void test01()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> iterator_type;
  typedef char_traits<wchar_t> traits;

  bool test = true;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk("en_HK");
  locale loc_fr("fr_FR@euro");
  locale loc_de("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the __timepunct facets, for quicker gdb inspection
  const __timepunct<wchar_t>& time_c = use_facet<__timepunct<wchar_t> >(loc_c); 
  const __timepunct<wchar_t>& time_de = use_facet<__timepunct<wchar_t> >(loc_de); 
  const __timepunct<wchar_t>& time_hk = use_facet<__timepunct<wchar_t> >(loc_hk); 
  const __timepunct<wchar_t>& time_fr = use_facet<__timepunct<wchar_t> >(loc_fr); 

  // create an ostream-derived object, cache the time_put facet
  const wstring empty;
  wostringstream oss;
  const time_put<wchar_t>& tim_put = use_facet<time_put<wchar_t> >(oss.getloc()); 

  // create "C" time objects
  tm time1 = { 0, 0, 12, 4, 3, 71 };
  const wchar_t* all = L"%a %A %b %B %c %d %H %I %j %m %M %p %s %U "
    		       L"%w %W %x %X %y %Y %Z %%";
  const wchar_t* date = L"%A, the second of %B";
  const wchar_t* date_ex = L"%Ex";

  // 1
  // iter_type 
  // put(iter_type s, ios_base& str, char_type fill, const tm* t,
  //	 char format, char modifier = 0) const;
  oss.str(empty);
  oss.imbue(loc_c);
  iterator_type os_it01 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  wstring result1 = oss.str();
  VERIFY( result1 == L"Sun" );

  oss.str(empty);
  iterator_type os_it21 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  wstring result21 = oss.str(); // "04/04/71"
  oss.str(empty);
  iterator_type os_it22 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  wstring result22 = oss.str(); // "12:00:00"
  oss.str(empty);
  iterator_type os_it31 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  wstring result31 = oss.str(); // "04/04/71"
  oss.str(empty);
  iterator_type os_it32 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  wstring result32 = oss.str(); // "12:00:00"

  oss.str(empty);
  oss.imbue(loc_de);
  iterator_type os_it02 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  wstring result2 = oss.str();
  VERIFY( result2 == L"Son" );

  oss.str(empty); // "%d.%m.%Y"
  iterator_type os_it23 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  wstring result23 = oss.str(); // "04.04.1971"
  oss.str(empty); // "%T"
  iterator_type os_it24 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  wstring result24 = oss.str(); // "12:00:00"
  oss.str(empty);
  iterator_type os_it33 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  wstring result33 = oss.str(); // "04.04.1971"
  oss.str(empty);
  iterator_type os_it34 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  wstring result34 = oss.str(); // "12:00:00"

  oss.str(empty);
  oss.imbue(loc_hk);
  iterator_type os_it03 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  wstring result3 = oss.str();
  VERIFY( result3 == L"Sun" );

  oss.str(empty); // "%A, %B %d, %Y"
  iterator_type os_it25 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  wstring result25 = oss.str(); // "Sunday, April 04, 1971"
  oss.str(empty); // "%I:%M:%S %Z"
  iterator_type os_it26 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  wstring result26 = oss.str(); // "12:00:00 PST"
  oss.str(empty);
  iterator_type os_it35 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  wstring result35 = oss.str(); // "Sunday, April 04, 1971"
  oss.str(empty);
  iterator_type os_it36 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  wstring result36 = oss.str(); // "12:00:00 PST"

  oss.str(empty);
  oss.imbue(loc_fr);
  iterator_type os_it04 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  wstring result4 = oss.str();
  VERIFY( result4 == L"dim" );

  oss.str(empty); // "%d.%m.%Y"
  iterator_type os_it27 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  wstring result27 = oss.str(); // "04.04.1971"
  oss.str(empty); // "%T"
  iterator_type os_it28 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  wstring result28 = oss.str(); // "12:00:00"
  oss.str(empty);
  iterator_type os_it37 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  wstring result37 = oss.str(); // "04.04.1971"
  oss.str(empty);
  iterator_type os_it38 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  wstring result38 = oss.str(); // "12:00:00"

  // 2
  oss.str(empty);
  oss.imbue(loc_c);
  iterator_type os_it05 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + traits::length(date));
  wstring result5 = oss.str();
  VERIFY( result5 == L"Sunday, the second of April");
  iterator_type os_it06 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + traits::length(date));
  wstring result6 = oss.str();
  VERIFY( result6 != result5 );

  oss.str(empty);
  oss.imbue(loc_de);
  iterator_type os_it07 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + traits::length(date));
  wstring result7 = oss.str();
  VERIFY( result7 == L"Sonntag, the second of April");
  iterator_type os_it08 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + traits::length(date));
  wstring result8 = oss.str();
  VERIFY( result8 != result7 );

  oss.str(empty);
  oss.imbue(loc_hk);
  iterator_type os_it09 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + traits::length(date));
  wstring result9 = oss.str();
  VERIFY( result9 == L"Sunday, the second of April");
  iterator_type os_it10 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + traits::length(date));
  wstring result10 = oss.str();
  VERIFY( result10 != result9 );

  oss.str(empty);
  oss.imbue(loc_fr);
  iterator_type os_it11 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + traits::length(date));
  wstring result11 = oss.str();
  VERIFY( result11 == L"dimanche, the second of avril");
  iterator_type os_it12 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + traits::length(date));
  wstring result12 = oss.str();
  VERIFY( result12 != result11 );
}
#endif

int main()
{
#ifdef _GLIBCPP_USE_WCHAR_T
  test01();
#endif
  return 0;
}
