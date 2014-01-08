// 1999-09-20 bkoz

// Copyright (C) 1999-2014 Free Software Foundation, Inc.
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


// 27.4.3 template class fpos

#include <cwchar> // for mbstate_t
#include <ios>
#include <cstring>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::mbstate_t state_type;
  state_type state01 = state_type();
  state_type state02 = state_type();

  std::streampos pos01(0);
  std::streampos pos02(0);

  // 27.4.3.1 fpos members
  // void state(state_type s);
  // state_type state();

  // XXX Need to have better sanity checking for the mbstate_t type,
  // or whatever the insantiating type for class fpos happens to be
  // for streampos, as things like equality operators and assignment
  // operators, increment and deincrement operators need to be in
  // place.
  pos01.state(state02);
  state01 = pos01.state();
  test = std::memcmp(&state01, &state02, sizeof(state_type)) == 0;
  VERIFY( test );
}

int main() 
{
  test01();
  return 0;
}
