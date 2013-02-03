// { dg-require-namedlocale "de_DE" }

// 2003-12-19  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

// 22.2.2.1.1  num_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;
  
  bool test __attribute__((unused)) = true;

  // A locale that expects grouping  
  locale loc_de = locale("de_DE");
  wistringstream iss;
  iss.imbue(loc_de);

  const num_get<wchar_t>& ng = use_facet<num_get<wchar_t> >(iss.getloc()); 
  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  double d = 0.0;
  double d1 = 1e1;
  double d2 = 3e1;
  
  iss.str(L"1e1,");
  end = ng.get(iss.rdbuf(), 0, iss, err, d);
  VERIFY( err == ios_base::goodbit );
  VERIFY( *end == L',' );
  VERIFY( d == d1 );

  iss.str(L"3e1.");
  iss.clear();
  err = ios_base::goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, d);
  VERIFY( err == ios_base::goodbit );
  VERIFY( *end == L'.' );
  VERIFY( d == d2 );
}

int main()
{
  test01();
  return 0;
}
