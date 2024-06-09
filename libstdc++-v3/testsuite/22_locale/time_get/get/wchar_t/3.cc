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

  wistringstream iss;
  iss.imbue(loc_c);
  const time_get<wchar_t>& tget = use_facet<time_get<wchar_t>>(iss.getloc());
  typedef istreambuf_iterator<wchar_t> iter;
  const iter end;

  tm time;
  ios_base::iostate err = ios_base::badbit;

  // PR78714 tests
  iss.str(L"Mon");
  wstring format = L"%a";
  auto ret = tget.get(iter(iss), end, iss, err, &time,
		      format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 1 );

  iss.str(L"Tue L");
  format = L"%a";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == L' ' );
  VERIFY( time.tm_wday == 2 );

  iss.str(L"Wednesday");
  format = L"%a";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 3 );

  iss.str(L"Thu");
  format = L"%A";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 4 );

  iss.str(L"Fri L");
  format = L"%A";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == L' ' );
  VERIFY( time.tm_wday == 5 );

  iss.str(L"Saturday");
  format = L"%A";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_wday == 6 );

  iss.str(L"Feb");
  format = L"%b";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 1 );

  iss.str(L"Mar L");
  format = L"%b";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == L' ' );
  VERIFY( time.tm_mon == 2 );

  iss.str(L"April");
  format = L"%b";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 3 );

  iss.str(L"May");
  format = L"%B";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 4 );

  iss.str(L"Jun L");
  format = L"%B";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == L' ' );
  VERIFY( time.tm_mon == 5 );

  iss.str(L"July");
  format = L"%B";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 6 );

  iss.str(L"Aug");
  format = L"%h";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 7 );

  iss.str(L"May L");
  format = L"%h";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret != end && *ret == L' ' );
  VERIFY( time.tm_mon == 4 );

  iss.str(L"October");
  format = L"%h";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 9 );

  // Other tests.
  iss.str(L" 1.");
  format = L"%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 1 );

  iss.str(L"2.");
  format = L"%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 2 );

  iss.str(L"03.");
  format = L"%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 3 );

  iss.str(L"0.");
  format = L"%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == L'.' );

  iss.str(L"32.");
  format = L"%d.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == L'2' );

  iss.str(L" 4.");
  format = L"%e.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 4 );

  iss.str(L"5.");
  format = L"%e.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 5 );

  iss.str(L"06.");
  format = L"%e.";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::goodbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mday == 6 );

  iss.str(L"0");
  format = L"%e";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit|ios_base::eofbit );
  VERIFY( ret == end );

  iss.str(L"35");
  format = L"%e";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == L'5' );

  iss.str(L" \t\t 02");
  format = L"%t%m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 1 );

  iss.str(L" \t \t 03");
  format = L"%n%m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 2 );

  iss.str(L" \t \t 4");
  format = L" %m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 3 );

  iss.str(L" \t \t 5");
  format = L"\t%m";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_mon == 4 );

  iss.str(L"12:00AM");
  format = L"%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 0 );
  VERIFY( time.tm_min == 0 );

  iss.str(L"12:37AM");
  format = L"%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 0 );
  VERIFY( time.tm_min == 37 );

  iss.str(L"01:25AM");
  format = L"%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 1 );
  VERIFY( time.tm_min == 25 );

  iss.str(L"12:00PM");
  format = L"%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 12 );
  VERIFY( time.tm_min == 0 );

  iss.str(L"12:42PM");
  format = L"%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 12 );
  VERIFY( time.tm_min == 42 );

  iss.str(L"07:23PM");
  format = L"%I:%M%p";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 19 );
  VERIFY( time.tm_min == 23 );

  iss.str(L"17%20");
  format = L"%H%%%M";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 17 );
  VERIFY( time.tm_min == 20 );

  iss.str(L"24:30");
  format = L"%H:%M";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == L'4' );

  // This one behaves differently from strptime, in a single
  // pass scaning we can't go back.
  iss.str(L"Novembur");
  format = L"%bembur";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::failbit );
  VERIFY( ret != end && *ret == L'u' );
}

int
main()
{
  test01();
  return 0;
}
