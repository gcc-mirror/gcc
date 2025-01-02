// { dg-do run { target c++17 } }

// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// C++17 30.10.8.4.3 path appends [fs.path.append]

#include <filesystem>
#include <string_view>
#include <testsuite_fs.h>
#include <testsuite_iterators.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

// path::operator/=(const Source& source)
// path::append(const Source& source)
// Equivalent to: return operator/=(path(source));

// path::append(InputIterator first, InputIterator last)
// Equivalent to: return operator/=(path(first, last));

template<typename Char>
void test(const path& p, const Char* s)
{
  path expected = p;
  expected /= path(s);

  path oper = p;
  oper /= s;

  path func = p;
  func.append(s);

  __gnu_test::test_container<const Char, __gnu_test::input_iterator_wrapper>
    input_range(s, s + std::char_traits<Char>::length(s));
  path range = p;
  range.append(input_range.begin(), input_range.end());

  compare_paths( oper, expected );
  compare_paths( func, expected );
  compare_paths( range, expected );
}

void
test01()
{
  test( "/foo/bar", "/foo/" );

  test( "baz", "baz" );
  test( "baz/", "baz" );
  test( "baz", "/foo/bar" );
  test( "baz/", "/foo/bar" );

  test( "", "" );
  test( "", "rel" );

  test( "dir/", "/file" );
  test( "dir/", "file" );
}

void
test02()
{
  // C++17 [fs.path.append] p4
  test( "//host", "foo" );
  test( "//host/", "foo" );
  test( "foo", "" );
  test( "foo", "/bar" );
  test( "foo", "c:/bar" );
  test( "foo", "c:" );
  test( "c:", "" );
  test( "c:foo", "/bar" );
  test( "foo", "c:\\bar" );
}

void
test03()
{
  for (const path p : __gnu_test::test_paths)
    for (const path q : __gnu_test::test_paths)
    {
      test(p, q.c_str());
      if constexpr (!std::is_same_v<path::value_type, char>)
	test(p, q.string().c_str());
    }
}

void
test04()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  test(  "foo", L"/bar" );
  test( L"foo",  "/bar" );
  test( L"foo", L"/bar" );
#endif
}

void
test05()
{
  std::basic_string_view<path::value_type> s;

  path p = "0/1/2/3/4/5/6";
  // The string_view aliases the path's internal string:
  s = p.native();
  path::string_type expected(s);
  expected += path::preferred_separator;
  expected += s;
  // Append that string_view, which must work correctly even though the
  // internal string will be reallocated during the operation:
  p /= s;
  compare_paths(p, expected);

  // Same again with a trailing slash:
  path p2 = "0/1/2/3/4/5/";
  s = p2.native();
  expected = s;
  expected += s;
  p2 /= s;
  compare_paths(p2, expected);

  // And aliasing one of the components of the path:
  path p3 = "0/123456789/a";
  path::iterator second = std::next(p3.begin());
  s = second->native();
  expected = p3.native() + path::preferred_separator;
  expected += s;
  p3 /= s;
  compare_paths(p3, expected);
}

void
test06()
{
  const std::string s0 = "a/b/c";
  path p = s0;
  std::string s;
  for (int i = 0; i < 10; ++i)
    s += "0/1/2/3/4/5/6/7/8/9/";
  // append a long string with many components
  test(p, s.c_str());

  // Same again but with a trailing slash on the left operand:
  path p2 = s0 + '/';
  test(p2, s.c_str());
}

void
test07()
{
  path p, p0;
  std::string_view s;
  p /= s; // PR libstdc++/97167
  compare_paths(p, p0);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
}
