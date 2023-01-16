// 2010-01-05  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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
#include <cstring>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction
  locale loc_c = locale::classic();

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  wistringstream iss;
  iss.imbue(loc_c);
  const time_get<wchar_t>& tim_get
    = use_facet<time_get<wchar_t> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // iter_type 
  // get_monthname(iter_type, iter_type, ios_base&, 
  //               ios_base::iostate&, tm*) const

  // sanity checks for "C" locale

  const wchar_t* amname[12] = { L"Jan", L"Feb", L"Mar", L"Apr",
				L"May", L"Jun", L"Jul", L"Aug",
				L"Sep", L"Oct", L"Nov", L"Dec" };

  for (int i = 0; i < 12; ++i)
    {
      iss.str(amname[i]);
      iterator_type is_it01(iss);
      tm time01;
      memset(&time01, -1, sizeof(tm));
      errorstate = good;
      tim_get.get_monthname(is_it01, end, iss, errorstate, &time01);
      VERIFY( time01.tm_mon == i );
      VERIFY( errorstate == ios_base::eofbit );
    }

  const wchar_t* mname[12] = { L"January", L"February", L"March",
			       L"April", L"May", L"June",
			       L"July", L"August", L"September",
			       L"October", L"November", L"December" };

  for (int i = 0; i < 12; ++i)
    {
      iss.str(mname[i]);
      iterator_type is_it01(iss);
      tm time01;
      memset(&time01, -1, sizeof(tm));
      errorstate = good;
      tim_get.get_monthname(is_it01, end, iss, errorstate, &time01);
      VERIFY( time01.tm_mon == i );
      VERIFY( errorstate == ios_base::eofbit );
    }
}

int main()
{
  test01();
  return 0;
}
