// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // Check time_get works with other iterators besides streambuf
  // input iterators.
  typedef string::const_iterator iter_type;
  typedef time_get<char, iter_type> time_get_type;
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = goodbit;
  const locale loc_c = locale::classic();

  // Create "C" time objects
  const tm time_sanity = __gnu_test::test_tm(0, 0, 12, 26, 5, 97, 2, 0, 0);
  tm tm1;

  istringstream iss; 
  iss.imbue(locale(loc_c, new time_get_type));

  // Iterator advanced, state, output.
  const time_get_type& tg = use_facet<time_get_type>(iss.getloc());

  // Cindy Sherman's Untitled Film Stills
  // June 26-September 2, 1997
  const string str = "1997 Cindy Sherman";
 
  // 05 get_year
  string res5;
  err = goodbit;
  // White space is not eaten, so manually increment past it.
  iter_type end5 = tg.get_year(str.begin(), str.end(), iss, err, &tm1);
  string rem5(end5, str.end());
  VERIFY( err == goodbit );
  VERIFY( tm1.tm_year == time_sanity.tm_year );
  VERIFY( rem5 ==  " Cindy Sherman" );
}

int main()
{
  test03();
  return 0;
}
