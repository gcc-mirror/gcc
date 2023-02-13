// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-options "-O2" }
// { dg-do run }

// This test checks that operator>> will avoid a buffer overflow when
// reading into a buffer with a size that is known at compile time.

// Since C++20 this is guaranteed (see LWG 2499), for previous standards
// checking the buffer size is an extension and depends on optimisation.

#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::istringstream in("foolishly");
  char pc[5];
  in >> pc;
  VERIFY( in.good() );
  VERIFY( std::string(pc) == "fool" );

#if __cplusplus <= 201703L
  char* p = pc + 1;
  in >> p;
  VERIFY( in.good() );
  VERIFY( std::string(pc) == "fish" );

  p = pc + 4;
  *p = '#';
  in >> p;
  VERIFY( in.fail() ); // if no characters are extracted, failbit is set
  VERIFY( *p == '\0' );
#endif
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
  test02();
  test03();
}
