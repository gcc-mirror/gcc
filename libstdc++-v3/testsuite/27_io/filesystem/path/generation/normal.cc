// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
compare_paths(path p, std::string expected)
{
#if defined(_WIN32) && !defined(__CYGWIN__)
  for (auto& c : expected)
    if (c == '/')
      c = '\\';
#endif
  __gnu_test::compare_paths(p, expected);
}

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

  // PR libstdc++/82777
  compare_paths( path("./a/b/c/../.././b/c").lexically_normal(), "a/b/c" );
  compare_paths( path("/a/b/c/../.././b/c").lexically_normal(), "/a/b/c" );
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
    {"/foo/.."     , "/" },
    {"/foo/../.."  , "/" },
    {"/foo/bar/.." , "/foo/" },
    {"/foo/bar/../.." , "/" },
    {"/foo/bar/baz/../../.." , "/" }, // PR libstdc++/87116

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
    {"foo/../../..", "../.." },

    // with root name (OS-dependent):
#if defined(_WIN32) && !defined(__CYGWIN__)
    {"C:bar/.."    , "C:" },
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
    {"../.."        , "../.." },
    {"../../."      , "../.." },
    {".././../."    , "../.." },
    {".././.././"   , "../.." },
    {"/.."          , "/" },
  };
  for (auto& test : testcases)
    compare_paths( path(test.input).lexically_normal(), test.normalized );
}

void
test04()
{
  // PR libstdc++/87116
  path p = "a/b/c";
  compare_paths( (p/"../..").lexically_normal(), "a/" );

  p = "a/b/c/d/e";
  compare_paths( (p/"..").lexically_normal(),  "a/b/c/d/" );
  compare_paths( (p/"../..").lexically_normal(),  "a/b/c/" );
  compare_paths( (p/"../../..").lexically_normal(),  "a/b/" );
  compare_paths( (p/"../../../..").lexically_normal(),  "a/" );
  compare_paths( (p/"../../../../..").lexically_normal(),  "." );
  compare_paths( (p/"../../../../../..").lexically_normal(),  ".." );

  p = "/a/b/c/d/e";
  compare_paths( (p/"..").lexically_normal(),  "/a/b/c/d/" );
  compare_paths( (p/"../..").lexically_normal(),  "/a/b/c/" );
  compare_paths( (p/"../../..").lexically_normal(),  "/a/b/" );
  compare_paths( (p/"../../../..").lexically_normal(),  "/a/" );
  compare_paths( (p/"../../../../..").lexically_normal(),  "/" );
  compare_paths( (p/"../../../../../..").lexically_normal(),  "/" );

#if defined(_WIN32) && !defined(__CYGWIN__)
  p = "A:b/c/d/e";
  compare_paths( (p/"..").lexically_normal(),  "A:b/c/d/" );
  compare_paths( (p/"../..").lexically_normal(),  "A:b/c/" );
  compare_paths( (p/"../../..").lexically_normal(),  "A:b/" );
  compare_paths( (p/"../../../..").lexically_normal(),  "A:" );
  compare_paths( (p/"../../../../..").lexically_normal(),  "A:.." );
  compare_paths( (p/"../../../../../..").lexically_normal(),  "A:../.." );

  p = "A:/b/c/d/e";
  compare_paths( (p/"..").lexically_normal(),  "A:/b/c/d/" );
  compare_paths( (p/"../..").lexically_normal(),  "A:/b/c/" );
  compare_paths( (p/"../../..").lexically_normal(),  "A:/b/" );
  compare_paths( (p/"../../../..").lexically_normal(),  "A:/" );
  compare_paths( (p/"../../../../..").lexically_normal(),  "A:/" );
  compare_paths( (p/"../../../../../..").lexically_normal(),  "A:/" );
#endif
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
