// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

// 27.4.3 fpos

#include <typeinfo>
#include <ios>
#include <testsuite_hooks.h>

// libstdc++/12065
void test01()
{
  std::streampos p(0);
  const std::streampos q(0);
  std::streamoff o(0);

  VERIFY( typeid(p + o) == typeid(std::streampos) );
  VERIFY( typeid(q + o) == typeid(std::streampos) );
  VERIFY( typeid(p - o) == typeid(std::streampos) );
  VERIFY( typeid(q - o) == typeid(std::streampos) );
  VERIFY( typeid(p - q) == typeid(std::streamoff) );
  VERIFY( typeid(q - p) == typeid(std::streamoff) );
}

int main()
{
  test01();
  return 0;
}
