// { dg-do run { target c++17 } }

// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

// NB: This file is predicated on sstreams, istreams, and ostreams
// working, not to mention other major details like char_traits, and
// all of the string_view class.

#include <string_view>
#include <stdexcept>
#include <sstream>
#include <fstream>
#include <iostream>
#include <testsuite_hooks.h>

void
test01()
{
  typedef std::string_view::size_type csize_type;
  typedef std::string_view::const_reference cref;
  typedef std::string_view::reference ref;

  const std::string_view str01("sailing grand traverse bay\n"
	       "\t\t\t    from Elk Rapids to the point reminds me of miles");
    
  // ostream& operator<<(ostream&, const basic_string_view&)
  std::ostringstream ostrs01;
  try 
    {
      ostrs01 << str01;
      VERIFY( ostrs01.str() == str01 );
    }
  catch(std::exception& fail) 
    {
      VERIFY( false );
    }
  
  std::string_view hello_world;
  std::cout << hello_world;
}

int
main()
{ 
  test01();

  return 0;
}
