// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

void
test01()
{
  // C++17 [fs.path.gen] p2
  compare_paths( path("foo/./bar/..").lexically_normal(), "foo/" );
  compare_paths( path("foo/.///bar/../").lexically_normal(), "foo/" );
}

void
test02()
{
  compare_paths( path("foo/../bar").lexically_normal(), "bar" );
  compare_paths( path("../foo/../bar").lexically_normal(), "../bar" );
  compare_paths( path("foo/../").lexically_normal(), "." );
  compare_paths( path("../../").lexically_normal(), "../.." );
  compare_paths( path("../").lexically_normal(), ".." );
  compare_paths( path("./").lexically_normal(), "." );
  compare_paths( path().lexically_normal(), "" );

  compare_paths( path("/..").lexically_normal(), "/" );
}

void
test03()
{
  struct
  {
    const char* input;
    const char* normalized;
  } testcases[] = {
    {""            , "" },
    {"."           , "."  },
    {".."          , ".." },
    {"/"           , "/" },
    {"//"          , "//" },

    {"/foo"        , "/foo" },
    {"/foo/"       , "/foo/" },
    {"/foo/."      , "/foo/" },
    {"/foo/bar/.." , "/foo/" },
    {"/foo/.."     , "/" },

    {"/."          , "/" },
    {"/./"         , "/" },
    {"/./."        , "/" },
    {"/././"       , "/" },
    {"/././."      , "/" },

    {"./"          , "." },
    {"./."         , "." },
    {"././"        , "." },
    {"././."       , "." },
    {"./././"      , "." },
    {"./././."     , "." },

    {"foo/.."      , "." },
    {"foo/../"     , "." },
    {"foo/../.."   , ".." },

    // with root name (OS-dependent):
#if defined(_WIN32) && !defined(__CYGWIN__)
    {"C:bar/.."    , "C:." },
#else
    {"C:bar/.."    , "." },
#endif
    {"C:/bar/.."   , "C:/" },
    {"C:"          , "C:" },
#ifdef __CYGWIN__
    {"//host/bar/.." , "//host/" },
    {"//host"        , "//host" },
#else
    {"//host/bar/.." , "/host/" },
    {"//host"        , "/host" },
#endif

    // a few others:
    {"foo/../foo/.."   , "." },
    {"foo/../foo/../.."   , ".." },
    {"../foo/../foo/.."   , ".." },
    {"../.f/../f"   , "../f" },
    {"../f/../.f"   , "../.f" },
    {".././../."    , "../.." },
    {".././.././"   , "../.." },
    {"/.."          , "/" },
  };
  for (auto& test : testcases)
    compare_paths( path(test.input).lexically_normal(), test.normalized );
}

int
main()
{
  test01();
  test02();
  test03();
}
