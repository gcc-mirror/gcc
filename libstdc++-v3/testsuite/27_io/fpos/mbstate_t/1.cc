// 1999-09-20 bkoz

// Copyright (C) 1999-2024 Free Software Foundation, Inc.
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
  typedef std::mbstate_t state_type;
  // Use zero-initialization of the underlying memory so that padding
  // bytes, if any, stand a better chance of comparing the same.
  // Zero-initialized memory is guaranteed to be a valid initial
  // state.  This doesn't quite guarantee that any padding bits won't
  // be overwritten when copying from other instances that haven't
  // been fully initialized: this data type is compatible with C, so
  // it is likely plain old data, but it could have a default ctor
  // that initializes only the relevant fields, whereas copy-ctor and
  // operator= could be implemented as a full-object memcpy, including
  // padding bits, rather than fieldwise copying.  However, since
  // we're comparing two values copied from the same state_type
  // instance, if padding bits are copied, we'll get the same for both
  // of them, and if they aren't, we'll keep the values we initialized
  // them with, so this should be good.
  state_type state[2];
  std::memset(state, 0, sizeof (state));


  std::streampos pos01(0);

  // 27.4.3.1 fpos members
  // void state(state_type s);
  // state_type state();

  // XXX Need to have better sanity checking for the mbstate_t type,
  // or whatever the instantiating type for class fpos happens to be
  // for streampos, as things like equality operators and assignment
  // operators, increment and deincrement operators need to be in
  // place.
  pos01.state(state[1]);
  state[0] = pos01.state();
  VERIFY( std::memcmp(&state[0], &state[1], sizeof(state_type)) == 0 );
}

int main() 
{
  test01();
  return 0;
}
