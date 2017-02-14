// 981208 bkoz test functionality of basic_stringbuf for char_type == char

// Copyright (C) 1997-2017 Free Software Foundation, Inc.
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

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/3955 -- ios_base::app overwrites from the beginning
void test05()
{
  std::ostringstream os ("foo");
  os << "bar";

  VERIFY( os.str() == "bar" );
}

int main()
{
  test05();
  return 0;
}



// more candy!!!
