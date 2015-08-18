// Copyright (C) 2015 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11 -lstdc++fs" }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <testsuite_hooks.h>

using std::experimental::filesystem::path;

void
test01()
{
  VERIFY( exists(path{"/"}) );
  VERIFY( exists(path{"/."}) );
  VERIFY( exists(path{"."}) );
}

void
test02()
{
  path rel{"xXxXx"};
  while (exists(rel))
    rel /= "x";
  VERIFY( !exists(rel) );
}

void
test03()
{
  path abs{"/xXxXx"};
  while (exists(abs))
    abs /= "x";
  VERIFY( !exists(abs) );
}

int
main()
{
  test01();
  test02();
  test03();
}
