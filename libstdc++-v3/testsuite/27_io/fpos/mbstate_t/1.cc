// 1999-09-20 bkoz

// Copyright (C) 1999, 2001, 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 27.4.3 template class fpos

#include <cwchar> // for mbstate_t
#include <ios>
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
  test = memcmp(&state01, &state02, sizeof(state_type)) == 0;
  VERIFY( test );
}

int main() 
{
  test01();
  return 0;
}
