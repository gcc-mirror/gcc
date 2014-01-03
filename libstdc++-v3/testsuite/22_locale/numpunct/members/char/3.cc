// { dg-require-namedlocale "it_IT" }

// 2001-01-24 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// 22.2.3.2 Template class numpunct_byname

#include <locale>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  
  bool test __attribute__((unused)) = true;

  locale loc_it = locale("it_IT");

  const numpunct<char>& nump_it = use_facet<numpunct<char> >(loc_it); 

  string g = nump_it.grouping();

  VERIFY( g == "" );
}

int main()
{
  test02();
  return 0;
}
