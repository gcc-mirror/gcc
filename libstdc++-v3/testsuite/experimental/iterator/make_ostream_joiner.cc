// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/iterator>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::ostringstream os;
  auto joiner = std::experimental::make_ostream_joiner(os, "...");
  for (int i : { 1, 2, 3, 4, 5 })
    *joiner++ = i;
  VERIFY( os.str() == "1...2...3...4...5" );
}

int
main()
{
  test01();
}
