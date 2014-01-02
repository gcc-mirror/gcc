// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

// 27.7.3.2 member functions (ostringstream_members)

#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  std::wostringstream os01;
  const std::wstring str00; 
  const std::wstring str01 = L"123";
  std::wstring str02;

  // string str() const
  str02 = os01.str();
  VERIFY( str00 == str02 );

  // void str(const basic_string&)
  os01.str(str01);
  str02 = os01.str();
  VERIFY( str01 == str02 );
}

int main()
{
  test01();
  return 0;
}
