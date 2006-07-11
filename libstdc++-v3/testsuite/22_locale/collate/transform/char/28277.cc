// 2006-07-11  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.4.1.1 collate members

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/28277
void test01()
{
  using namespace std;
  typedef collate<char>::string_type string_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();

  // cache the collate facets
  const collate<char>& coll_c = use_facet<collate<char> >(loc_c); 

  const string_type sstr(10000000, 'a');

  const string_type dstr = coll_c.transform(sstr.data(),
					    sstr.data() + sstr.size());

  VERIFY( dstr == sstr );
}

int main()
{
  test01();
  return 0;
}
