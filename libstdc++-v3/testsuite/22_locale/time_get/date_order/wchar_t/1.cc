// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction
  locale loc_c = locale::classic();

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
