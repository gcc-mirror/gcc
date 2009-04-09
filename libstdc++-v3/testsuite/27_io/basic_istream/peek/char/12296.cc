// Copyright (C) 2003, 2009 Free Software Foundation
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

// 27.6.1.3 unformatted input functions

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/12296
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  istringstream stream;
  VERIFY( stream.rdstate() == ios_base::goodbit );
  istringstream::int_type c = stream.peek();
  VERIFY( c == istringstream::traits_type::eof() );
  VERIFY( stream.rdstate() == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
