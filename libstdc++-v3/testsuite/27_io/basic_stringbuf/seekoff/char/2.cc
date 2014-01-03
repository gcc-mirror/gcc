// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// 27.7.1.3 Overridden virtual functions

#include <sstream>
#include <testsuite_hooks.h>

void test02(std::stringbuf& in, bool pass)
{
  bool test __attribute__((unused)) = true;
  using namespace std;
  typedef streambuf::pos_type pos_type;
  typedef streambuf::off_type off_type;
  pos_type bad = pos_type(off_type(-1));
  pos_type p = 0;

  // seekoff
  p = in.pubseekoff(0, ios_base::beg, ios_base::in);
  if (pass)
    VERIFY( p != bad );

  p = in.pubseekoff(0, ios_base::beg, ios_base::out); 
  VERIFY( p == bad );

  p = in.pubseekoff(0, ios_base::beg); 
  VERIFY( p == bad );
}

int main() 
{
  using namespace std;

  // movie star, submarine scientist!
  stringbuf in1("Hedy Lamarr", ios_base::in);
  stringbuf in2(ios_base::in);
  stringbuf in3("", ios_base::in);
  test02(in1, true);
  test02(in2, false);
  test02(in3, false);
  return 0;
}
