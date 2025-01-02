// { dg-require-namedlocale "fr_FR.ISO8859-15" }

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

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test08()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;
  typedef char_traits<char> traits;

  // create "C" time objects
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);
  const char* date = "%A, the second of %B";
  const char* date_ex = "%Ex";

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_fr = locale(ISO_8859(15,fr_FR));
  VERIFY( loc_fr != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const string empty;
  ostringstream oss;
  oss.imbue(loc_fr);
  const time_put<char>& tim_put = use_facet<time_put<char> >(oss.getloc()); 
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 
	      date, date + traits::length(date));
  string result11 = oss.str();
  VERIFY( result11 == "dimanche, the second of avril");
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 
	      date_ex, date_ex + traits::length(date_ex));
  string result12 = oss.str();
  VERIFY( result12 != result11 );
}

int main()
{
  test08();
  return 0;
}
