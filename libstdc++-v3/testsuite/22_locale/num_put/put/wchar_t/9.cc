// 2004-08-22  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// According to 22.2.2.2.2, p23 and the resolution of DR 359
// val must be casted to a signed type: this can be revealed
// by ios_base::showpos, which is effective only for signed
// types (also see libstdc++/15565 about this).
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();

  // sanity check the data is correct.
  const wstring empty;

  // cache the num_put facet
  wostringstream oss;
  oss.imbue(loc_c);
  const num_put<wchar_t>& np = use_facet<num_put<wchar_t> >(oss.getloc());

  bool b = true;
  np.put(oss.rdbuf(), oss, L' ', b);
  VERIFY( oss.str() == L"1" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios_base::showpos);
  np.put(oss.rdbuf(), oss, L' ', b);
  VERIFY( oss.str() == L"+1" );
}

int main()
{
  test01();
  return 0;
}
