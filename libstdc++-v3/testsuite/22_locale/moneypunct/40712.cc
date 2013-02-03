// { dg-require-namedlocale "en_US" }

// 2009-07-18  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

// 22.2.6.3  Template class moneypunct

#include <locale>

// libstdc++/40712
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  locale loc(locale("C"), "en_US", locale::monetary);
  
  use_facet<moneypunct<char> >(loc).grouping();
}

int main()
{
  test01();
  return 0;
}
