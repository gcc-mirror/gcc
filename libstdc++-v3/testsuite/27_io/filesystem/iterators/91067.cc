// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-do link { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>

void
test01()
{
  std::filesystem::directory_iterator d;
  d = d;
  d = std::move(d);
}

void
test02()
{
  std::filesystem::recursive_directory_iterator d;
  d = d;
  d = std::move(d);
}

void
test03()
{
  std::filesystem::directory_iterator d;
  auto d2 = std::move(d);
}

void
test04()
{
  std::filesystem::recursive_directory_iterator d;
  auto d2 = std::move(d);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
