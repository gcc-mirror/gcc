// { dg-options "-std=gnu++11" }

//
// 2013-07-17  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013 Free Software Foundation, Inc.
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

// 28.11.3 regex_search
// Tests BRE against a std::string target.

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::regex  re("as\\(df\\)", std::regex::basic);
  std::string target("xxasdfyy");
  std::smatch m;

  VERIFY( std::regex_search(target, m, re) );

  VERIFY( m.size()  == re.mark_count()+1 );
  VERIFY( m.empty() == false );
  VERIFY( m.prefix().matched == (m.prefix().first != m.prefix().second) );
  VERIFY( std::string(m.prefix().first, m.prefix().second) == "xx" );
  VERIFY( m.suffix().matched == (m.suffix().first != m.suffix().second) );
  VERIFY( std::string(m.suffix().first, m.suffix().second) == "yy" );
  VERIFY( m[0].matched == true );
  VERIFY( std::string(m[0].first, m[0].second) == "asdf" );
  VERIFY( m[1].matched == true );
  VERIFY( std::string(m[1].first, m[1].second) == "df" );
}

int
main()
{
  test01();
  return 0;
}
