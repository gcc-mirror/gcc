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

// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// C++17 30.10.15.33 Resize file [fs.op.resize_file]

#include <filesystem>
#include <string>
#include <fstream>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  auto p = __gnu_test::nonexistent_path();
  std::error_code ec;
  resize_file(p, 0, ec);
  VERIFY( ec );
  ec = {};
  resize_file(p, 1, ec);
  VERIFY( ec );

  __gnu_test::scoped_file f(p);
  std::ofstream{p} << "some text";
  std::ifstream fin;
  std::string input;

#ifdef _GLIBCXX_HAVE_TRUNCATE
  resize_file(p, 4, ec);
  VERIFY( !ec );
  fin.open(p);
  getline(fin, input);
  VERIFY( input.length() == 4 );
  fin.close();

  resize_file(p, 2);
  fin.open(p);
  getline(fin, input);
  VERIFY( input.length() == 2 );
  fin.close();
#endif

  resize_file(p, 0, ec);
  VERIFY( !ec );
  fin.open(p);
  getline(fin, input);
  VERIFY( input.length() == 0 );
  fin.close();
}

int
main()
{
  test01();
}
