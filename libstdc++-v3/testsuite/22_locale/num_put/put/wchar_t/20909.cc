// 2005-04-09  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005 Free Software Foundation
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

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/20909
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // A locale that expects grouping.
  locale loc_de = __gnu_test::try_named_locale("de_DE");

  const wstring empty;
  wstring result;

  wostringstream oss;
  oss.imbue(loc_de);
  const num_put<wchar_t>& np = use_facet<num_put<wchar_t> >(oss.getloc()); 

  double d0 = 2e20;
  double d1 = -2e20;

  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, L'*', d0);
  result = oss.str();
  VERIFY( result == L"2e+20" );

  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, L'*', d1);
  result = oss.str();
  VERIFY( result == L"-2e+20" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios::uppercase);
  np.put(oss.rdbuf(), oss, L'*', d0);
  result = oss.str();
  VERIFY( result == L"2E+20" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios::showpos);
  np.put(oss.rdbuf(), oss, L'*', d0);
  result = oss.str();
  VERIFY( result == L"+2E+20" );
}

int main()
{
  test01();
  return 0;
}


