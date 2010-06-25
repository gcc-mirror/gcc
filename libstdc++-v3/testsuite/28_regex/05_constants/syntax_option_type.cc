// { dg-options "-std=c++0x" }
// { dg-do compile }
//
// 2009-06-17  Stephen M. Webb  <stephen.webb@xandros.com>
//
// Copyright (C) 2009 Free Software Foundation, Inc.
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

// 28.5.1 

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

	std::regex_constants::syntax_option_type option = 0;

	option |= std::regex_constants::icase;
	option |= std::regex_constants::nosubs;
	option |= std::regex_constants::optimize;
	option |= std::regex_constants::collate;
	option |= std::regex_constants::ECMAScript;
	option |= std::regex_constants::basic;
	option |= std::regex_constants::extended;
	option |= std::regex_constants::awk;
	option |= std::regex_constants::grep;
	option |= std::regex_constants::egrep;
}

int main()
{
  test01();
  return 0;
}
