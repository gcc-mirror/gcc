// 2003-02-05 Paolo Carlini <pcarlini@unitus.it>

// Copyright (C) 2003, 2009 Free Software Foundation
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

// libstdc++/9548 and DR 231
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wostringstream woss1, woss2;
  const num_put<wchar_t>& np1 = use_facet<num_put<wchar_t> >(woss1.getloc());
  const num_put<wchar_t>& np2 = use_facet<num_put<wchar_t> >(woss2.getloc());

  wstring result1, result2;

  woss1.precision(-1);
  woss1.setf(ios_base::fixed, ios_base::floatfield);
  np1.put(woss1.rdbuf(), woss1, L'+', 30.5);
  result1 = woss1.str();
  VERIFY( result1 == L"30.500000" );

  woss2.precision(0);
  woss2.setf(ios_base::scientific, ios_base::floatfield);
  np2.put(woss2.rdbuf(), woss2, L'+', 1.0);
  result2 = woss2.str();
  VERIFY( result2 == L"1e+00" );
}

int main()
{
  test01();
  return 0;
}
