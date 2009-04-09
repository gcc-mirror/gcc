// 2003-12-03  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003, 2004, 2009 Free Software Foundation
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

// libstdc++/12791
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  wistringstream iss;
  const time_get<wchar_t>& tim_get =
    use_facet<time_get<wchar_t> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  iss.str(L"60/04/71");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  iterator_type ret01 = tim_get.get_date(is_it01, end, iss, errorstate,
					 &time01);
  VERIFY( errorstate == ios_base::failbit );
  VERIFY( *ret01 == L'6' );

  iss.str(L"04/38/71");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = good;
  iterator_type ret02 = tim_get.get_date(is_it02, end, iss, errorstate,
					 &time02);
  VERIFY( errorstate == ios_base::failbit );
  VERIFY( *ret02 == L'8' );
}

int main()
{
  test01();
  return 0;
}
