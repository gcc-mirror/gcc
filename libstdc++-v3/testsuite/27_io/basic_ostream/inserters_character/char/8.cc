// 1999-08-16 bkoz

// Copyright (C) 1999-2020 Free Software Foundation, Inc.
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

// 27.6.2.5.4 basic_ostream character inserters

#include <string>
#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

void test08()
{
  char* pt = 0;

  // 1
  std::ostringstream oss;
  oss << pt;
  VERIFY( oss.bad() );
  VERIFY( oss.str().size() == 0 );

  oss.clear();
  oss << "";
  VERIFY( oss.good() );
}

int main()
{
  test08();
  return 0;
}
