// { dg-options "-std=gnu++11" }

//
// 2013-08-01  Tim Shen <timshen91@gmail.com>
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

// 28.11.2 regex_match
// Tests Extended bracket expression against a C-string.

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  {
    std::regex  re("pre/[za-x]", std::regex::extended);
    VERIFY( std::regex_match("pre/z", re) );
    VERIFY( std::regex_match("pre/a", re) );
    VERIFY( !std::regex_match("pre/y", re) );
  }
  {
    std::regex  re("pre/[[:uPPer:]]", std::regex::extended);
    VERIFY( std::regex_match("pre/Z", re) );
    VERIFY( !std::regex_match("pre/_", re) );
    VERIFY( !std::regex_match("pre/a", re) );
    VERIFY( !std::regex_match("pre/0", re) );
  }
  {
    std::regex  re("pre/[[:lOWer:]]", std::regex::extended | std::regex::icase);
    VERIFY( std::regex_match("pre/Z", re) );
    VERIFY( std::regex_match("pre/a", re) );
  }
  {
    std::regex  re("pre/[[:w:][.tilde.]]", std::regex::extended);
    VERIFY( std::regex_match("pre/~", re) );
    VERIFY( std::regex_match("pre/_", re) );
    VERIFY( std::regex_match("pre/a", re) );
    VERIFY( std::regex_match("pre/0", re) );
  }
}

int
main()
{
  test01();
  return 0;
}
