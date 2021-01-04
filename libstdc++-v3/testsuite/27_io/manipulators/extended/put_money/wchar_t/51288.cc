// { dg-do run { target c++11 } }
// { dg-require-namedlocale "en_US.UTF-8" }

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

// libstdc++/51288
void test01()
{
  std::locale loc_us = std::locale("en_US.UTF-8");

  std::wostringstream oss;
  oss.imbue(loc_us);

  const std::wstring str(L"123");

  oss.setstate(std::ios_base::failbit);

  oss << std::put_money(str);

  VERIFY( oss.str().empty() );
  VERIFY( oss.fail() );
}

int main()
{
  test01();
  return 0;
}
