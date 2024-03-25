// { dg-do run { target c++11 } }

// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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
#include <iterator>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;

  locale loc_c = locale::classic();

  istringstream iss;
  iss.imbue(loc_c);
  const time_get<char>& tget = use_facet<time_get<char>>(iss.getloc());
  typedef istreambuf_iterator<char> iter;
  const iter end;

  tm time;
  ios_base::iostate err = ios_base::badbit;

  // PR78714 tests
  iss.str("Mon");
  string format = "%a";
  auto ret = tget.get(iter(iss), end, iss, err, &time,
		      format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 1 );

  iss.str("Tue ");
  format = "%a";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == ' ' );
  VERIFY( time.tm_wday == 2 );

  iss.str("Wednesday");
  format = "%a";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 3 );

  iss.str("Thu");
  format = "%A";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 4 );

  iss.str("Fri ");
  format = "%A";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == ' ' );
  VERIFY( time.tm_wday == 5 );

  iss.str("Saturday");
  format = "%A";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 6 );

  iss.str("Feb");
  format = "%b";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 1 );

  iss.str("Mar ");
  format = "%b";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == ' ' );
  VERIFY( time.tm_mon == 2 );

  iss.str("April");
  format = "%b";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 3 );

  iss.str("May");
  format = "%B";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 4 );

  iss.str("Jun ");
  format = "%B";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == ' ' );
  VERIFY( time.tm_mon == 5 );

  iss.str("July");
  format = "%B";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 6 );

  iss.str("Aug");
  format = "%h";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 7 );

  iss.str("May ");
  format = "%h";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == ' ' );
  VERIFY( time.tm_mon == 4 );

  iss.str("October");
  format = "%h";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 9 );

  // Other tests.
  iss.str(" 1.");
  format = "%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 1 );

  iss.str("2.");
  format = "%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 2 );

  iss.str("03.");
  format = "%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 3 );

  iss.str("0.");
  format = "%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == '.' );

  iss.str("32.");
  format = "%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == '2' );

  iss.str(" 4.");
  format = "%e.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 4 );

  iss.str("5.");
  format = "%e.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 5 );

  iss.str("06.");
  format = "%e.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 6 );

  iss.str("0");
  format = "%e";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit|ios_base::eofbit );
  VERIFY( ret == end );

  iss.str("35");
  format = "%e";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == '5' );

  iss.str(" \t\t 02");
  format = "%t%m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 1 );

  iss.str(" \t \t 03");
  format = "%n%m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 2 );

  iss.str(" \t \t 4");
  format = " %m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 3 );

  iss.str(" \t \t 5");
  format = "\t%m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 4 );

  iss.str("12:00AM");
  format = "%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 0 );
  VERIFY( time.tm_min == 0 );

  iss.str("12:37AM");
  format = "%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 0 );
  VERIFY( time.tm_min == 37 );

  iss.str("01:25AM");
  format = "%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 1 );
  VERIFY( time.tm_min == 25 );

  iss.str("12:00PM");
  format = "%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 12 );
  VERIFY( time.tm_min == 0 );

  iss.str("12:42PM");
  format = "%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 12 );
  VERIFY( time.tm_min == 42 );

  iss.str("07:23PM");
  format = "%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 19 );
  VERIFY( time.tm_min == 23 );

  iss.str("17%20");
  format = "%H%%%M";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 17 );
  VERIFY( time.tm_min == 20 );

  iss.str("24:30");
  format = "%H:%M";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == '4' );

  // This one behaves differently from strptime, in a single
  // pass scaning we can't go back.
  iss.str("Novembur");
  format = "%bembur";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == 'u' );
}

int
main()
{
  test01();
  return 0;
}
