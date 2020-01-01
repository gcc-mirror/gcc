// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// 8.4.8 path compare [path.compare]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

int norm(int i)
{
  if (i < 0)
    return -1;
  else if (i > 0)
    return +1;
  else
    return 0;
}

void
check(const path& lhs, const path& rhs, int sense)
{
  VERIFY( lhs.compare(lhs) == 0 );
  VERIFY( rhs.compare(rhs) == 0 );

  VERIFY( norm(lhs.compare(rhs)) == sense );
  VERIFY( norm(lhs.compare(rhs.c_str())) == sense );

  VERIFY( norm(rhs.compare(lhs)) == -sense );
  VERIFY( norm(rhs.compare(lhs.c_str())) == -sense );
}

void
test01()
{
  check("", "", 0);

  // These are root names on Windows (just relative paths elsewhere)
  check("", "c:", -1);
  check("c:", "d:", -1);
  check("c:", "c:/", -1);
  check("d:", "c:/", +1);
#if defined(__MING32__) || defined(__MINGW64__)
  check("c:/a/b", "c:a/b", +1);
#else
  check("c:/a/b", "c:a/b", -1);
#endif

  // These are root names on Cygwin (just relative paths elsewhere)
  check("", "//c", -1);
  check("//c", "//d", -1);
  check("//c", "//c/", -1);
  check("//d", "//c/", +1);

  check("a", "/", -1);
  check("/a", "/b", -1);
  check("a", "/b", -1);
  check("/b", "b", +1);
}

int
main()
{
  test01();
}
