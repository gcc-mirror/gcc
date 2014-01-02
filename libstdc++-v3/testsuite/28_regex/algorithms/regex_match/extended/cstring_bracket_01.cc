// { dg-options "-std=gnu++11" }

//
// 2013-08-01  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  bool test __attribute__((unused)) = true;

  {
    std::regex  re("pre/[za-x]", std::regex::extended);
    VERIFY( regex_match_debug("pre/z", re) );
    VERIFY( regex_match_debug("pre/a", re) );
    VERIFY( !regex_match_debug("pre/y", re) );
  }
  {
    std::regex  re("pre/[[:uPPer:]]", std::regex::extended);
    VERIFY( regex_match_debug("pre/Z", re) );
    VERIFY( !regex_match_debug("pre/_", re) );
    VERIFY( !regex_match_debug("pre/a", re) );
    VERIFY( !regex_match_debug("pre/0", re) );
  }
  {
    std::regex  re("pre/[[:lOWer:]]", std::regex::extended | std::regex::icase);
    VERIFY( regex_match_debug("pre/Z", re) );
    VERIFY( regex_match_debug("pre/a", re) );
  }
  {
    std::regex  re("pre/[[:w:][.tilde.]]", std::regex::extended);
    VERIFY( regex_match_debug("pre/~", re) );
    VERIFY( regex_match_debug("pre/_", re) );
    VERIFY( regex_match_debug("pre/a", re) );
    VERIFY( regex_match_debug("pre/0", re) );
  }
  {
    std::regex  re("pre/[[=a=]]", std::regex::extended);
    VERIFY( regex_match_debug("pre/a", re) );
    VERIFY( regex_match_debug("pre/A", re) );
  }
}

int
main()
{
  test01();
  return 0;
}
