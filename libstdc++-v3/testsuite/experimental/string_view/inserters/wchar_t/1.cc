// { dg-do run { target c++14 } }

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

// inserters

// NB: This file is predicated on sstreams, and ostreams
// working, not to mention other major details like char_traits, and
// all of the string_view class.

#include <experimental/string_view>
#include <stdexcept>
#include <sstream>
#include <fstream>
#include <iostream>
#include <testsuite_hooks.h>

void
test01()
{
  typedef std::experimental::wstring_view::size_type csize_type;
  typedef std::experimental::wstring_view::const_reference cref;
  typedef std::experimental::wstring_view::reference ref;

  const std::experimental::wstring_view str01(L"sailing grand traverse bay\n"
	       L"\t\t\t    from Elk Rapids to the point reminds me of miles");
  const std::experimental::wstring_view str02(L"sailing");
  const std::experimental::wstring_view str03(L"grand");
  const std::experimental::wstring_view str04(L"traverse");
  const std::experimental::wstring_view str05;
  std::experimental::wstring_view str10;

  // ostream& operator<<(ostream&, const basic_string_view&)
  std::wostringstream ostrs01;
  try 
    {
      ostrs01 << str01;
      VERIFY( ostrs01.str() == str01 );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false );
    }
  
  std::experimental::wstring_view hello_world;
  std::wcout << hello_world;
}

int
main()
{ 
  test01();

  return 0;
}
