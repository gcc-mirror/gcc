// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-options "-O2 -std=gnu++98" }
// { dg-do run }

// This test checks that operator>> will avoid a buffer overflow when
// reading into a buffer with a size that is known at compile time.

// Since C++20 this is guaranteed (see LWG 2499), for previous standards
// we try to check the buffer size as an extension (which depends on -O2).

#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::istringstream in("foolish child");
  char pc[5];
  in >> pc;
  VERIFY( in.good() );
  VERIFY( std::string(pc) == "fool" );
}

void
test02()
{
  std::istringstream in("foolish");
  signed char sc[5];
  in >> sc;
  VERIFY( in.good() );
  VERIFY( std::string((const char*)sc) == "fool" );
}

void
test03()
{
  std::istringstream in("foolish");
  unsigned char uc[5];
  in >> uc;
  VERIFY( in.good() );
  VERIFY( std::string((const char*)uc) == "fool" );
}

int
main()
{
  test01();
}
