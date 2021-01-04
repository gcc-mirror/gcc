// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

#include <streambuf>
#include <istream>
#include <testsuite_hooks.h>

struct SB : std::streambuf
{
  virtual int_type underflow() { throw 1; }
};

void
test01()
{
  SB sb;
  std::istream is(&sb);
  int i;
  is >> i;
  VERIFY( is.bad() );
}

int
main()
{
  test01();
}
