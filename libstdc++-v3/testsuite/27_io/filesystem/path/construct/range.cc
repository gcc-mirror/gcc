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

// 8.4.1 path constructors [path.construct]

#include <filesystem>
#include <string>
#include <testsuite_fs.h>
#include <testsuite_iterators.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

void
test01()
{
  for (std::string s : __gnu_test::test_paths)
  {
    path p1 = s;
    path p2( s.begin(), s.end() );
    path p3( s.c_str() );
    path p4( s.c_str(), s.c_str() + s.size() );

    compare_paths(p1, p2);
    compare_paths(p1, p3);
    compare_paths(p1, p4);

#if _GLIBCXX_USE_WCHAR_T
    std::wstring ws(s.begin(), s.end());
    path p5 = ws;
    path p6( ws.begin(), ws.end() );
    path p7( ws.c_str() );
    path p8( ws.c_str(), ws.c_str() + ws.size() );

    compare_paths(p1, p5);
    compare_paths(p1, p6);
    compare_paths(p1, p7);
    compare_paths(p1, p8);
#endif

    using __gnu_test::test_container;
    using __gnu_test::input_iterator_wrapper;
    // Test with input iterators and const value_types

    test_container<char, input_iterator_wrapper>
      r1((char*)s.c_str(), (char*)s.c_str() + s.size());
    path p9(r1.begin(), r1.end());
    compare_paths(p1, p9);

    test_container<char, input_iterator_wrapper>
      r2((char*)s.c_str(), (char*)s.c_str() + s.size() + 1); // includes null-terminator
    path p10(r2.begin());
    compare_paths(p1, p10);

    test_container<const char, input_iterator_wrapper>
      r3(s.c_str(), s.c_str() + s.size());
    path p11(r3.begin(), r3.end());
    compare_paths(p1, p11);

    test_container<const char, input_iterator_wrapper>
      r4(s.c_str(), s.c_str() + s.size() + 1); // includes null-terminator
    path p12(r4.begin());
    compare_paths(p1, p12);

#if _GLIBCXX_USE_WCHAR_T
    // Test with input iterators and const value_types
    test_container<wchar_t, input_iterator_wrapper>
      r5((wchar_t*)ws.c_str(), (wchar_t*)ws.c_str() + ws.size());
    path p13(r5.begin(), r5.end());
    compare_paths(p1, p13);

    test_container<wchar_t, input_iterator_wrapper>
      r6((wchar_t*)ws.c_str(), (wchar_t*)ws.c_str() + ws.size() + 1); // includes null-terminator
    path p14(r6.begin());
    compare_paths(p1, p14);

    test_container<const wchar_t, input_iterator_wrapper>
      r7(ws.c_str(), ws.c_str() + ws.size());
    path p15(r7.begin(), r7.end());
    compare_paths(p1, p15);

    test_container<const wchar_t, input_iterator_wrapper>
      r8(ws.c_str(), ws.c_str() + ws.size() + 1); // includes null-terminator
    path p16(r8.begin());
    compare_paths(p1, p16);
#endif
  }
}

int
main()
{
  test01();
}
