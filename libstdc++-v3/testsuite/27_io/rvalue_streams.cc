// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <sstream>
#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  int i = 1742;
  // This usage isn't supported by the current draft.
  // std::string result = (std::ostringstream() << i).str();
  std::ostringstream() << i;
  std::string result ("1742");
  int i2;
  std::istringstream(result) >> i2;
  VERIFY (i == i2);
}

struct X { bool as_rvalue; };

void operator>>(std::istream&, X& x) { x.as_rvalue = false; }
void operator>>(std::istream&, X&& x) { x.as_rvalue = true; }

// LWG 2328 Rvalue stream extraction should use perfect forwarding
void
test02()
{
  X x;
  std::istringstream is;
  auto& ref1 = (std::move(is) >> x);
  VERIFY( &ref1 == &is );
  VERIFY( x.as_rvalue == false );
  auto& ref2 = (std::move(is) >> std::move(x));
  VERIFY( &ref2 == &is );
  VERIFY( x.as_rvalue == true );

  char arr[2];
  std::istringstream("x") >> &arr[0];
  std::istringstream("x") >> arr;
}

int
main()
{
  test01();
  test02();
  return 0;
}
