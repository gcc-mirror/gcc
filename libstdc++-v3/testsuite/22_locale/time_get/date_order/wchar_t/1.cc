// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation
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
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk = __gnu_test::try_named_locale("en_HK");
  locale loc_fr = __gnu_test::try_named_locale("fr_FR@euro");
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  const string empty;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;
  istringstream iss;
  iss.imbue(loc_c);
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc()); 

  // 1
  // dateorder date_order() const
  iss.imbue(loc_c);
  dateorder do1 = tim_get.date_order();
  //  VERIFY( do1 == time_base::mdy );
  VERIFY( do1 == time_base::no_order );
}

int main()
{
  test01();
  return 0;
}
