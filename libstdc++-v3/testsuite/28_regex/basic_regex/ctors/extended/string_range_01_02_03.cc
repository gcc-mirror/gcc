// { dg-do run { target c++11 } }

//
// 2010-06-16  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.8.2 basic_regex ctor
// Tests for invalid range expression

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  try
    {
      std::regex  re("a{1,2,3}", std::regex::extended);
    }
  catch (std::regex_error& ex)
    {
      VERIFY( ex.code() == std::regex_constants::error_brace );
    }
}


int
main()
{ 
  test01();
  return 0;
}

