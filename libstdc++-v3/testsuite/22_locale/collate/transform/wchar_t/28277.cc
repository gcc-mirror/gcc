// 2006-07-11  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2018 Free Software Foundation, Inc.
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

// 22.2.4.1.1 collate members

// { dg-options "-DMAX_SIZE=100000" { target simulator } }

#ifndef MAX_SIZE
#define MAX_SIZE 10000000
#endif

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/28277
void test01()
{
  using namespace std;
  typedef collate<wchar_t>::string_type string_type;

  // basic construction
  locale loc_c = locale::classic();

  // cache the collate facets
  const collate<wchar_t>& coll_c = use_facet<collate<wchar_t> >(loc_c); 

  const string_type sstr(MAX_SIZE, L'a');

  const string_type dstr = coll_c.transform(sstr.data(),
					    sstr.data() + sstr.size());

  VERIFY( dstr == sstr );
}

int main()
{
  test01();
  return 0;
}
