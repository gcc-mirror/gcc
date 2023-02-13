// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

#include <experimental/buffer>
#include <testsuite_hooks.h>

using std::experimental::net::const_buffer;
using std::experimental::net::mutable_buffer;

void
test01()
{
  bool test __attribute__((unused)) = false;
  char c[4];

  mutable_buffer mb;
  VERIFY( buffer_size(mb) == 0 );

  mb = mutable_buffer(c, sizeof(c));
  VERIFY( buffer_size(mb) == mb.size() );

  const_buffer cb;
  VERIFY( buffer_size(cb) == 0 );
  cb = const_buffer(c, sizeof(c));
  VERIFY( buffer_size(cb) == cb.size() );
}

void
test02()
{
  bool test __attribute__((unused)) = false;
  char c[32];

  std::vector<mutable_buffer> mv{ {c, 0}, {c, 32}, {c, 16}, {c, 3}, {c, 0} };
  VERIFY( buffer_size(mv) == (0 + 32 + 16 + 3 + 0) );

  std::vector<const_buffer> cv{ {c, 0}, {c, 32}, {c, 16}, {c, 3}, {c, 0} };
  VERIFY( buffer_size(cv) == (0 + 32 + 16 + 3 + 0) );
}

int
main()
{
  test01();
  test02();
}
