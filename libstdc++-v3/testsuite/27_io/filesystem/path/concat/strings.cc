// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// 30.10.7.4.4 path concatenation [fs.path.concat]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using std::filesystem::path;

void
test01()
{
  path p("/");
  p += "foo";
  VERIFY( p.filename().string() == "foo" );
  p += "bar";
  VERIFY( p.filename().string() == "foobar" );
  p += '/';
  VERIFY( p.parent_path().string() == "/foobar" );
  VERIFY( p.filename().string() == "" );
#if _GLIBCXX_USE_WCHAR_T
  VERIFY( p.parent_path().wstring() == L"/foobar" );
  VERIFY( p.filename().wstring() == L"" );
  p += L"baz.txt";
#else
  p += "baz.txt";
#endif
  VERIFY( p.filename().string() == "baz.txt" );
  p.concat("/dir/");
  // N.B. on Windows p.parent_path() is "/foobar\\baz.txt\\dir"
  VERIFY( p.parent_path() == path("/foobar/baz.txt/dir")  );
  VERIFY( p.filename().string() == "" );
  const char file[] = "file";
  __gnu_test::test_container<const char, __gnu_test::input_iterator_wrapper>
    input(file, file + 4);
  p.concat(input.begin(), input.end());
  VERIFY( p.filename().string() == file );
}

void
test02()
{
  std::basic_string_view<path::value_type> s;

  path p = "0/1/2/3/4/5/6";
  // The string_view aliases the path's internal string:
  s = p.native();
  // Append that string_view, which must work correctly even though the
  // internal string will be reallocated during the operation:
  p += s;
  VERIFY( p.string() == "0/1/2/3/4/5/60/1/2/3/4/5/6" );

  // Same again with a trailing slash:
  path p2 = "0/1/2/3/4/5/";
  s = p2.native();
  p2 += s;
  VERIFY( p2.string() == "0/1/2/3/4/5/0/1/2/3/4/5/" );

  // And aliasing one of the components of the path:
  path p3 = "0/123456789";
  path::iterator second = std::next(p3.begin());
  s = second->native();
  p3 += s;
  VERIFY( p3.string() == "0/123456789123456789" );
}

int
main()
{
  test01();
  test02();
}
