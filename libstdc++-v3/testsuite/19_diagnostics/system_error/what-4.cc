// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007, 2008
// Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 19.1 Exception classes

#include <cstring>
#include <string>
#include <system_error>
#include <testsuite_hooks.h>

// Make sure each invocation of what() doesn't grow the message.
void test01()
{
  bool test __attribute__((unused)) = true;
  std::string s("after nine thirty, this request cannot be met");

  std::system_error obj = std::system_error(std::posix_error::invalid_argument, s);
  std::string s1(obj.what());
  std::string s2(obj.what());
  VERIFY( s1 == s2 );
}

int main(void)
{
  test01();
  return 0;
}
