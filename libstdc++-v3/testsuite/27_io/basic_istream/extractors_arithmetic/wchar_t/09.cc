// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.6.1.2.2 arithmetic extractors

// { dg-do run { xfail lax_strtofp } }

#include <istream>
#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

bool test09()
{
  bool test __attribute__((unused)) = true;

  std::wstring st(L"2.456e3-+0.567e-2");
  std::wstringbuf sb(st);
  std::wistream is(&sb);
  double f1 = 0, f2 = 0;
  wchar_t c;
  (is >> std::ws) >> f1;
  (is >> std::ws) >> c;
  (is >> std::ws) >> f2;
  test = f1 == 2456;
  VERIFY( f2 == 0.00567 );
  VERIFY( c == L'-' );
  return test;
}

int main()
{
  test09();
  return 0;
}
