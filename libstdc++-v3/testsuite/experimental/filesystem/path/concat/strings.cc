// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// 8.4.4 path concatenation [path.concat]

#include <experimental/filesystem>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using std::experimental::filesystem::path;

void
test01()
{
  path p("/");
  p += std::string("foo");
  VERIFY( p.filename().string() == "foo" );
  p += "bar";
  VERIFY( p.filename().string() == "foobar" );
  p += '/';
  VERIFY( p.parent_path().string() == "/foobar" );
  VERIFY( p.filename().string() == "." );
#if _GLIBCXX_USE_WCHAR_T
  VERIFY( p.parent_path().wstring() == L"/foobar" );
  VERIFY( p.filename().wstring() == L"." );
  p += L"baz.txt";
#else
  p += "baz.txt";
#endif
  VERIFY( p.filename().string() == "baz.txt" );
  p.concat("/dir/");
  // N.B. on Windows p.parent_path() is "/foobar\\baz.txt\\dir"
  VERIFY( p.parent_path() == path("/foobar/baz.txt/dir") );
  VERIFY( p.filename().string() == "." );
  const char file[] = "file";
  __gnu_test::test_container<const char, __gnu_test::input_iterator_wrapper>
    input(file, file + 4);
  p.concat(input.begin(), input.end());
  VERIFY( p.filename().string() == file );
}

int
main()
{
  test01();
}
