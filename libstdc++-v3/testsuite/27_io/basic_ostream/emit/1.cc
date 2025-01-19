// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-additional-options "-pthread" { target pthread } }
// { dg-do run { target c++20 } }
// { dg-require-effective-target cxx11_abi }
// { dg-require-effective-target rtti }

#include <syncstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::stringbuf sb;
  std::osyncstream s(&sb);
  s << "abc" << std::emit_on_flush << "def" << std::flush << "ghi"
    << std::emit_on_flush << std::noemit_on_flush << std::endl;
  VERIFY( sb.view() == "abcdef" );
  s << "jkl" << std::flush_emit << "mno" << std::flush;
  VERIFY( sb.view() == "abcdefghi\njkl" );
  s.emit();
  VERIFY( sb.view() == "abcdefghi\njklmno" );
}

int
main()
{
  test01();
}
