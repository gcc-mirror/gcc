// { dg-do run { target c++11 } }
//
// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <string>
#include <testsuite_hooks.h>

void test01()
{
  std::unordered_map<std::string, std::string> m
  { {"E", "E" }, { "T", "T" } };

  VERIFY( m.size() == 2 );

  auto bcount = m.bucket_count();

  m.insert({ {"E", "E" }, { "T", "T" } });

  VERIFY( m.size() == 2 );
  VERIFY( m.bucket_count() == bcount );
}

int main()
{
  test01();
  return 0;
}
