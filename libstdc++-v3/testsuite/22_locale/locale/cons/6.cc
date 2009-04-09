// 2001-01-19 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2001, 2003, 2009 Free Software Foundation
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

// 22.1.1 - Class locale [lib.locale]

#include <locale>
#include <string>
#include <testsuite_hooks.h>

// creating unnamed locales 1 using new + combine
void
test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  const string name_c("C");
  const string str_unnamed("*");
  string str;

  // construct a locale object with the specialized facet.
  locale		loc_c = locale::classic();
  locale 		loc_1(locale::classic(), new numpunct<char>);

  // check names
  VERIFY( loc_c.name() == name_c );
  VERIFY( loc_1.name() == str_unnamed );

  // sanity check the constructed locale has the specialized facet.
  VERIFY( has_facet<numpunct<char> >(loc_1) );
  VERIFY( has_facet<numpunct<char> >(loc_c) );
  
  // attempt to re-synthesize classic locale
  locale		loc_2 = loc_1.combine<numpunct<char> >(loc_c);
  VERIFY( loc_2.name() == str_unnamed );
  VERIFY( loc_2 != loc_c );
}

int main()
{
  test01();
  return 0;
}
