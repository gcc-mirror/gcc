// { dg-do run { target c++17 } }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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

#include <string_view>
#include <testsuite_hooks.h>

void
test01()
{
  using std::string_view;

  string_view str0{"olympus mons"};
  string_view::const_pointer p = str0.data();
  str0.remove_prefix(4);
  VERIFY( str0.data() == p + 4);
  VERIFY( str0.length() == 8 );
  VERIFY( str0 == string_view{"pus mons"} );
}

constexpr bool
test02()
{
  using std::string_view;

  string_view str0{"olympus mons"};
  string_view::const_pointer p = str0.data();
  str0.remove_prefix(4);
  if ( str0.data() != p + 4)
    return false;
  if ( str0.length() != 8 )
    return false;
  if ( str0 != string_view{"pus mons"} )
    return false;

  return true;
}

int
main()
{ 
  test01();
  static_assert( test02() );

  return 0;
}
