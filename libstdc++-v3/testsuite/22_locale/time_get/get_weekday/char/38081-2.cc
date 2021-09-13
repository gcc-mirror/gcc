// { dg-require-namedlocale "ru_RU.UTF-8" }

// 2010-01-05  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

  typedef istreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc("ru_RU.UTF-8");

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  iss.imbue(loc);
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // iter_type 
  // get_weekday(iter_type, iter_type, ios_base&, 
  //             ios_base::iostate&, tm*) const

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 14)
# if __GLIBC__ > 2 || __GLIBC_MINOR__ >= 17
  const char* awdays[7] = { "\u0412\u0441",
			    "\u041F\u043D",
			    "\u0412\u0442",
			    "\u0421\u0440",
			    "\u0427\u0442",
			    "\u041F\u0442",
			    "\u0421\u0431" };
# else
  const char* awdays[7] = { "\u0412\u0441\u002E",
			    "\u041F\u043D\u002E",
			    "\u0412\u0442\u002E",
			    "\u0421\u0440\u002E",
			    "\u0427\u0442\u002E",
			    "\u041F\u0442\u002E",
			    "\u0421\u0431\u002E" };
#endif
#else
  const char* awdays[7] = { "\u0412\u0441\u043A",
			    "\u041F\u043D\u0434",
			    "\u0412\u0442\u0440",
			    "\u0421\u0440\u0434",
			    "\u0427\u0442\u0432",
			    "\u041F\u0442\u043D",
			    "\u0421\u0431\u0442" };
#endif

  for (int i = 0; i < 7; ++i)
    {
      iss.str(awdays[i]);
      iterator_type is_it01(iss);
      tm time01;
      memset(&time01, -1, sizeof(tm));
      errorstate = good;
      tim_get.get_weekday(is_it01, end, iss, errorstate, &time01);
      VERIFY( time01.tm_wday == i );
      VERIFY( errorstate == ios_base::eofbit );
    }

  const char* wdays[7] = { "\u0412\u043E\u0441\u043A\u0440\u0435"
			   "\u0441\u0435\u043D\u044C\u0435",
			   "\u041F\u043E\u043D\u0435\u0434\u0435"
			   "\u043B\u044C\u043D\u0438\u043A",
			   "\u0412\u0442\u043E\u0440\u043D\u0438\u043A",
			   "\u0421\u0440\u0435\u0434\u0430",
			   "\u0427\u0435\u0442\u0432\u0435\u0440\u0433",
			   "\u041F\u044F\u0442\u043D\u0438\u0446\u0430",
			   "\u0421\u0443\u0431\u0431\u043E\u0442\u0430" };

  for (int i = 0; i < 7; ++i)
    {
      iss.str(wdays[i]);
      iterator_type is_it01(iss);
      tm time01;
      memset(&time01, -1, sizeof(tm));
      errorstate = good;
      tim_get.get_weekday(is_it01, end, iss, errorstate, &time01);
      VERIFY( time01.tm_wday == i );
      VERIFY( errorstate == ios_base::eofbit );
    }
}

int main()
{
  test01();
  return 0;
}
