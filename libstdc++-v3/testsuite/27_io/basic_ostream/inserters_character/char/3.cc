// 1999-08-16 bkoz

// Copyright (C) 1999-2021 Free Software Foundation, Inc.
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

// 27.6.2.5.4 basic_ostream character inserters

#include <string>
#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// width() != zero
// right
void test03(void) 
{
  std::string tmp;

  std::string str01 = "";
  std::ostringstream oss01;
  oss01.width(5);
  oss01.fill('0');
  oss01.flags(std::ios_base::right);
  oss01 << str01;
  tmp = oss01.str();
  VERIFY( tmp == "00000" );

  std::string str02 = "1";
  std::ostringstream oss02;
  oss02.width(5);
  oss02.fill('0');
  oss02.flags(std::ios_base::right);
  oss02 << str02;
  tmp = oss02.str();
  VERIFY( tmp == "00001" );

  std::string str03 = "909909";
  std::ostringstream oss03;
  oss03.width(5);
  oss03.fill('0');
  oss03.flags(std::ios_base::right);
  oss03 << str03;
  tmp = oss03.str();
  VERIFY( tmp == "909909" );
}

int main()
{
  test03();
  return 0;
}
