// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-add-options libatomic }
// { dg-do run { target c++2a } }

#include <stop_token>
#include <testsuite_hooks.h>

int lval[5];
int rval[5];

void
test01()
{
  std::stop_source ssrc;
  std::stop_token stok = ssrc.get_token();
  struct F
  {
    void operator()() const & { ++lval[i]; }
    void operator()() && { ++rval[i]; }

    int i;
  };
  std::stop_callback<F> cb0(stok, F{0});
  std::stop_callback<F> cb1(stok, F{1});
  std::stop_callback<F> cb2(stok, F{2});
  F f3{3};
  std::stop_callback<F&> cb3(stok, f3);
  std::stop_callback<const F> cb4(stok, F{4});

  // PR libstdc++/92895
  // Callback should be invoked with correct value category.
  ssrc.request_stop();

  VERIFY( lval[0] == 0 && lval[1] == 0 && lval[2] == 0 );
  VERIFY( lval[3] == 1 );
  VERIFY( lval[4] == 1 );
  VERIFY( rval[0] == 1 );
  VERIFY( rval[1] == 1 );
  VERIFY( rval[2] == 1 );
  VERIFY( rval[3] == 0 && rval[4] == 0 );
}

int main()
{
  test01();
}
