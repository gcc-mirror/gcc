// Copyright (C) 2006 Free Software Foundation, Inc.
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

#include <algorithm>
#include <iterator>
#include <sstream>
#include <testsuite_hooks.h>

struct no_assign
{
  int const x;
  no_assign() : x(23) { }
  operator int() const { return x; }
};

// libstdc++/26133
void test01()
{
  bool test __attribute__((unused)) = true;
  std::ostringstream oss1, oss2;

  no_assign in[4];

  std::unique_copy(in, in + 4, std::ostream_iterator<int>(oss1, "\n"));
  VERIFY( oss1.str() == "23\n" );

  std::unique_copy(in, in + 4, std::ostream_iterator<int>(oss2, "\n"),
		   std::equal_to<int>());
  VERIFY( oss2.str() == "23\n" );
}

int main()
{
  test01();
  return 0;
}
