// 1999-04-12 bkoz

// Copyright (C) 1999-2014 Free Software Foundation, Inc.
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

// 27.6.1.2.2 arithmetic extractors

// { dg-do run { xfail lax_strtofp } }

#include <istream>
#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

bool test09()
{
  bool test __attribute__((unused)) = true;

  std::string st("2.456e3-+0.567e-2");
  std::stringbuf sb(st);
  std::istream is(&sb);
  double f1 = 0, f2 = 0;
  char c;
  (is>>std::ws) >> f1;
  (is>>std::ws) >> c;
  (is>>std::ws) >> f2;
  test = f1 == 2456;
  VERIFY( f2 == 0.00567 );
  VERIFY( c == '-' );
  return test;
}

int main()
{
  test09();
  return 0;
}
