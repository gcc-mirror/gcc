// { dg-options "-std=c++0x" }

//
// 2010-06-16  Stephen M. Webb <stephen.webb@bregmasoft.ca>
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
// Tests BRE against a std::string target, exercising range {0,3}

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

	std::regex  re("a\\{0,3\\}", std::regex::basic);
	std::string target("aa");
	std::smatch m;

	VERIFY( std::regex_match(target, m, re) );

	VERIFY( m.size()  == re.mark_count()+1 );
	VERIFY( m.empty() == false );
	VERIFY( m.prefix().first == target.begin() );
	VERIFY( m.prefix().second == target.begin() );
	VERIFY( m.prefix().matched == false );
	VERIFY( m.suffix().first == target.end() );
	VERIFY( m.suffix().second == target.end() );
	VERIFY( m.suffix().matched == false );
	VERIFY( m[0].first == target.begin() );
	VERIFY( m[0].second == target.end() );
	VERIFY( m[0].matched == true );
}


int
main()
{ 
  test01();
  return 0;
}

