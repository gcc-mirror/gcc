// { dg-options "-std=gnu++1y" }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

// basic_string element access

#include <experimental/string_view>
#include <stdexcept>
#include <testsuite_hooks.h>

bool
test01()
{
  bool test [[gnu::unused]] = true;

  typedef std::experimental::string_view::size_type csize_type;
  typedef std::experimental::string_view::const_reference cref;
  typedef std::experimental::string_view::reference ref;
  csize_type csz01, csz02;

  const std::experimental::string_view str01("tamarindo, costa rica");
  std::experimental::string_view str02("41st street beach, capitola, california");
  std::experimental::string_view str03;

  // const_reference operator[] (size_type pos) const;
  csz01 = str01.size();
  cref cref1 = str01[csz01 - 1];
  VERIFY( cref1 == 'a' );
  cref cref2 = str01[csz01];
  VERIFY( cref2 == char() );

  // const_reference at(size_type pos) const;
  csz01 = str01.size();
  cref cref3 = str01.at(csz01 - 1);
  VERIFY( cref3 == 'a' );
  try
  {
    str01.at(csz01);
    VERIFY( false ); // Should not get here, as exception thrown.
  }
  catch (std::out_of_range& fail)
  {
    VERIFY( true );
  }
  catch (...)
  {
    VERIFY( false );
  }

  return test;
}

int
main()
{ 
  test01();
  return 0;
}
