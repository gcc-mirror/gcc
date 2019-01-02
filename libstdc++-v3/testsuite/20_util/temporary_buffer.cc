// 2002-01-24  Phil Edwards  <pme@gcc.gnu.org>

// Copyright (C) 2002-2019 Free Software Foundation, Inc.
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

// 20.4.3 temporary buffers

#include <memory>
#include <testsuite_hooks.h>

struct junk { char j[12]; };

int main(void)
{
  typedef std::pair<junk*, std::ptrdiff_t> pair_type;
  pair_type results = std::get_temporary_buffer<junk>(5);

  if (results.second != 0)
  {
      // make sure it works:  test the returned capacity, and then construct
      // some junk in the buffer.
      // XXX
      VERIFY( results.first != 0 );
  }
  else
  {
      // if it says it didn't work, make sure it didn't work
      VERIFY( results.first == 0 );
  }

  std::return_temporary_buffer(results.first);

  return 0;
}
