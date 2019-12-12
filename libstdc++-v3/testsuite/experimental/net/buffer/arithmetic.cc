// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

using std::experimental::net::mutable_buffer;
using std::experimental::net::const_buffer;

void
test01()
{
  bool test __attribute__((unused)) = false;
  char c[4];

  mutable_buffer mb;
  mb = mb + 0;
  VERIFY( mb.data() == nullptr );
  VERIFY( mb.size() == 0 );

  mb = 0 + mb;
  VERIFY( mb.data() == nullptr );
  VERIFY( mb.size() == 0 );

  mb = mutable_buffer(c, sizeof(c));
  mb = mb + 1;
  VERIFY( mb.data() == c+1 );
  VERIFY( mb.size() == 3 );

  mb = mb + 2;
  VERIFY( mb.data() == c+3 );
  VERIFY( mb.size() == 1 );

  mb = mb + 2;
  VERIFY( mb.data() == c+4 );
  VERIFY( mb.size() == 0 );

  mb = mutable_buffer(c, sizeof(c));
  mb = 3 + mb;
  VERIFY( mb.data() == c+3 );
  VERIFY( mb.size() == 1 );

  mb = 2 + mb;
  VERIFY( mb.data() == c+4 );
  VERIFY( mb.size() == 0 );
}

void
test02()
{
  bool test __attribute__((unused)) = false;
  char c[4];

  const_buffer cb;
  cb = cb + 0;
  VERIFY( cb.data() == nullptr );
  VERIFY( cb.size() == 0 );

  cb = 0 + cb;
  VERIFY( cb.data() == nullptr );
  VERIFY( cb.size() == 0 );

  cb = const_buffer(c, sizeof(c));
  cb = cb + 1;
  VERIFY( cb.data() == c+1 );
  VERIFY( cb.size() == 3 );

  cb = cb + 2;
  VERIFY( cb.data() == c+3 );
  VERIFY( cb.size() == 1 );

  cb = cb + 2;
  VERIFY( cb.data() == c+4 );
  VERIFY( cb.size() == 0 );

  cb = const_buffer(c, sizeof(c));
  cb = 3 + cb;
  VERIFY( cb.data() == c+3 );
  VERIFY( cb.size() == 1 );

  cb = 2 + cb;
  VERIFY( cb.data() == c+4 );
  VERIFY( cb.size() == 0 );
}

int
main()
{
  test01();
  test02();
}
