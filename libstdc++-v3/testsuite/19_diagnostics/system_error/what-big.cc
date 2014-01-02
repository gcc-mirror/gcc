// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

#include <string>
#include <system_error>
#include <testsuite_hooks.h>

// Can construct and return 10k character error string.
void test01()
{
  typedef std::system_error test_type;

  bool test __attribute__((unused)) = true;
  const std::string xxx(10000, 'x');
  test_type t(std::error_code(), xxx);
  VERIFY( std::string(t.what()).find(xxx) != std::string::npos );
}

int main(void)
{
  test01();
  return 0;
}
