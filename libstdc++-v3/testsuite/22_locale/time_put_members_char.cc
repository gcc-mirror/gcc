// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

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

// test string version
void test01()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

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

  // cache the moneypunct facets, for quicker gdb inspection
  const time_put<char>& timp_c = use_facet<time_put<char> >(loc_c); 
  const time_put<char>& timp_de = use_facet<time_put<char> >(loc_de); 
  const time_put<char>& timp_hk = use_facet<time_put<char> >(loc_hk); 
  const time_put<char>& timp_fr = use_facet<time_put<char> >(loc_fr); 

  // create an ostream-derived object, cache the time_put facet
  const string empty;
  ostringstream oss;
  const time_put<char>& tim_put = use_facet<time_put<char> >(oss.getloc()); 

  // create "C" time objects
  tm time1 = { 0, 0, 12, 4, 3, 71 };
  const char* all = "%a %A %b %B %c %d %H %I %j %m %M %p %s %U "
                    "%w %W %x %X %y %Y %Z %%";
  const char* date = "%A, the second of %B";
  const char* date_ex = "%Ex";

  // 1
  // iter_type 
  // put(iter_type s, ios_base& str, char_type fill, const tm* t,
  //	 char format, char modifier = 0) const;
  oss.str(empty);
  oss.imbue(loc_c);
  iterator_type os_it01 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result1 = oss.str();
  VERIFY( result1 == "Sun" );

  oss.str(empty);
  oss.imbue(loc_de);
  iterator_type os_it02 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result2 = oss.str();
  VERIFY( result2 == "Son" );

  oss.str(empty);
  oss.imbue(loc_hk);
  iterator_type os_it03 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result3 = oss.str();
  VERIFY( result3 == "Sun" );

  oss.str(empty);
  oss.imbue(loc_fr);
  iterator_type os_it04 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result4 = oss.str();
  VERIFY( result4 == "dim" );

  // 2
  oss.str(empty);
  oss.imbue(loc_c);
  iterator_type os_it05 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + strlen(date));
  string result5 = oss.str();
  VERIFY( result5 == "Sunday, the second of April");
  iterator_type os_it06 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + strlen(date));
  string result6 = oss.str();
  VERIFY( result6 != result5 );

  oss.str(empty);
  oss.imbue(loc_de);
  iterator_type os_it07 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + strlen(date));
  string result7 = oss.str();
  VERIFY( result7 == "Sonntag, the second of April");
  iterator_type os_it08 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + strlen(date));
  string result8 = oss.str();
  VERIFY( result8 != result7 );

  oss.str(empty);
  oss.imbue(loc_hk);
  iterator_type os_it09 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + strlen(date));
  string result9 = oss.str();
  VERIFY( result9 == "Sunday, the second of April");
  iterator_type os_it10 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + strlen(date));
  string result10 = oss.str();
  VERIFY( result10 != result9 );

  oss.str(empty);
  oss.imbue(loc_fr);
  iterator_type os_it11 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date, date + strlen(date));
  string result11 = oss.str();
  VERIFY( result11 == "dimanche, the second of avril");
  iterator_type os_it12 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 
				      date_ex, date_ex + strlen(date));
  string result12 = oss.str();
  VERIFY( result12 != result11 );
}

int main()
{
  test01();
  return 0;
}
