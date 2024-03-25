// { dg-do run { target c++11 } }
//
// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <string>
#include <testsuite_hooks.h>

void test01()
{
  {
    std::unordered_map<std::string, std::string> mymap;
    std::pair<std::string, std::string> mypair{std::string("key"),
	                                       std::string("value")};
    mymap.insert(mypair);

    VERIFY( mypair.first.length() && mypair.second.length() );
  }

  {
    std::unordered_map<std::string, std::string> mymap;
    std::pair<std::string, std::string> mypair{std::string("key"),
	                                       std::string("value")};
    mymap.insert(mymap.begin(), mypair);

    VERIFY( mypair.first.length() && mypair.second.length() );
  }
}

int main()
{
  test01();
  return 0;
}
