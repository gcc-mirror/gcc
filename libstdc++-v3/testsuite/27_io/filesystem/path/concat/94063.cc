// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-do run { target { *-*-*mingw* || *-*-cygwin } } }
// { dg-require-effective-target c++17 }

#include <filesystem>
#include <testsuite_hooks.h>

void
test01()
{
  using std::filesystem::path;
  path p;

  // PR libstdc++/94063
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  p = L"C";
  p += path(L":");
  VERIFY( p.has_root_name() );
  VERIFY( p.root_name() == p );
  p += path(L"\\");
  VERIFY( p.has_root_name() );
  VERIFY( p.has_root_directory() );
  VERIFY( p.root_name() == L"C:" );
  VERIFY( p.root_directory() == L"\\" );

  p = L"C";
  p += L':';
  VERIFY( p.has_root_name() );
  VERIFY( p.root_name() == p );
  p += L'\\';
  VERIFY( p.has_root_name() );
  VERIFY( p.has_root_directory() );
  VERIFY( p.root_name() == L"C:" );
  VERIFY( p.root_directory() == L"\\" );

  p = L"C:";
  p += path(L"/foo");
  VERIFY( p.has_root_name() );
  VERIFY( p.has_root_directory() );
  VERIFY( p.root_name() == L"C:" );
  VERIFY( p.root_directory() == L"/" );
  VERIFY( p.filename() == L"foo" );

  p = L"C:";
  p += L"/foo";
  VERIFY( p.has_root_name() );
  VERIFY( p.has_root_directory() );
  VERIFY( p.root_name() == L"C:" );
  VERIFY( p.root_directory() == L"/" );
  VERIFY( p.filename() == L"foo" );

  p = L"C";
  p += path(L":/foo");
  VERIFY( p.has_root_name() );
  VERIFY( p.has_root_directory() );
  VERIFY( p.root_name() == L"C:" );
  VERIFY( p.root_directory() == L"/" );
  VERIFY( p.filename() == L"foo" );

  p = L"C";
  p += L":/foo";
  VERIFY( p.has_root_name() );
  VERIFY( p.has_root_directory() );
  VERIFY( p.root_name() == L"C:" );
  VERIFY( p.root_directory() == L"/" );
  VERIFY( p.filename() == L"foo" );
#elif defined __CYGWIN__
  p = "/";
  p += path("/x");
  VERIFY( p.has_root_name() );
  VERIFY( p.root_name() == p );

  p = "/";
  p += "/x";
  VERIFY( p.has_root_name() );
  VERIFY( p.root_name() == p );

  p = "/";
  p += path("/");
  VERIFY( !p.has_root_name() );
  VERIFY( p.has_root_directory() );

  p = "/";
  p += "/";
  VERIFY( !p.has_root_name() );
  VERIFY( p.has_root_directory() );
#endif
}

int
main()
{
  test01();
}
