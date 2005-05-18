// { dg-require-namedlocale "" }

// 2003-12-17  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003 Free Software Foundation
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

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  locale loc_tw = locale("zh_TW");

  iterator_type end;

  wistringstream iss;
  iss.imbue(loc_tw);
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  const wchar_t wstr[] = { 0x897f, 0x5143, L'2', L'0', L'0', L'3',
			   0x5e74, L'1', L'2', 0x6708, L'1', L'7',
			   0x65e5 , 0x0 };

  iss.str(wstr);
  iterator_type is_it01(iss);
  tm time01;
  tim_get.get_date(is_it01, end, iss, errorstate, &time01);
  VERIFY( errorstate == ios_base::eofbit );
  VERIFY( time01.tm_mon == 11 );
  VERIFY( time01.tm_mday == 17 );
  VERIFY( time01.tm_year == 103 );
}

int main()
{
  test01();
  return 0;
}
