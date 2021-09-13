// 2004-04-07  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // basic construction
  locale loc_c = locale::classic();

  iterator_type end;
  wistringstream iss;
  iss.imbue(loc_c);
  const time_get<wchar_t>& tim_get =
    use_facet<time_get<wchar_t> >(iss.getloc()); 

  iss.str(L"Jul");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_monthname(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_mon == 6 );
  VERIFY( errorstate == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
