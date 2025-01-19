// { dg-do run { target c++17 } }

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

// 8.4.1 path constructors [path.construct]

#include <filesystem>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  path p;
  VERIFY(  p.empty() );
  VERIFY( !p.has_root_path() );
  VERIFY( !p.has_root_name() );
  VERIFY( !p.has_root_directory() );
  VERIFY( !p.has_relative_path() );
  VERIFY( !p.has_parent_path() );
  VERIFY( !p.has_filename() );
  VERIFY( !p.has_stem() );
  VERIFY( !p.has_extension() );
  VERIFY( !p.is_absolute() );
  VERIFY(  p.is_relative() );
  VERIFY( std::distance(p.begin(), p.end()) == 0 );
}

int
main()
{
  test01();
}
