// Copyright (C) 2004 Free Software Foundation
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.6.1.3 unformatted input functions

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/12296
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wistringstream stream;
  VERIFY( stream.rdstate() == ios_base::goodbit );
  wistringstream::int_type c = stream.peek();
  VERIFY( c == istringstream::traits_type::eof() );
  VERIFY( stream.rdstate() == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
