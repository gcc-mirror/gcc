// 2000-03-23 bkoz

// Copyright (C) 2000-2021 Free Software Foundation, Inc.
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
//

#include <sstream>
#include <ostream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef std::stringbuf::pos_type        pos_type;
  typedef std::stringbuf::off_type        off_type;

  // tellp
  ostringstream ost;
  pos_type pos1;
  pos1 = ost.tellp();
  VERIFY( pos1 == pos_type(off_type(0)) );
  ost << "RZA ";
  pos1 = ost.tellp();
  VERIFY( pos1 == pos_type(off_type(4)) );
  ost << "ghost dog: way of the samurai";
  pos1 = ost.tellp();
  VERIFY( pos1 == pos_type(off_type(33)) );
}                                    

int main()
{
  test01();
  return 0;
}
