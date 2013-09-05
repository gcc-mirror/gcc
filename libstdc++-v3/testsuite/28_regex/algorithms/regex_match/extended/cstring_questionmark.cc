// { dg-options "-std=c++0x" }

//
// 2010-06-21  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
// Tests ERE against a C-string target, question-mark match.

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::regex  re("(aa?)", std::regex::extended);
  char target[] = "a";
  std::cmatch m;

  VERIFY( std::regex_match(target, m, re) );

  VERIFY( re.mark_count() == 1 );
  VERIFY( m.size()  == re.mark_count()+1 );
  VERIFY( m.empty() == false );
  VERIFY( m.prefix().first == target );
  VERIFY( m.prefix().second == target );
  VERIFY( m.prefix().matched == false );
  VERIFY( m.suffix().first == target+sizeof(target)-1 );
  VERIFY( m.suffix().second == target+sizeof(target)-1 );
  VERIFY( m.suffix().matched == false );
  VERIFY( m[0].first == target );
  VERIFY( m[0].second == target+sizeof(target)-1 );
  VERIFY( m[0].matched == true );
  VERIFY( m[1].first == target );
  VERIFY( m[1].second == target+sizeof(target)-1 );
  VERIFY( m[1].matched == true );

  VERIFY(std::regex_match("", std::regex("a?", std::regex::extended)));
  VERIFY(std::regex_match("a", std::regex("a?", std::regex::extended)));
  VERIFY(!std::regex_match("aa", std::regex("a?", std::regex::extended)));
}


int
main()
{ 
  test01();
  return 0;
}

