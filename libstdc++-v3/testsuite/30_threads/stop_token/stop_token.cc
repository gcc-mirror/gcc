// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a -pthread" }
// { dg-do run }
// { dg-require-effective-target c++2a }
// { dg-require-effective-target pthread }

#include <stop_token>
#include <iostream>
#include <testsuite_hooks.h>

int main()
{
  // create stop_source
  std::stop_source ssrc;
  VERIFY(ssrc.stop_possible());
  VERIFY(!ssrc.stop_requested());

  // create stop_token from stop_source
  std::stop_token stok{ssrc.get_token()};
  VERIFY(ssrc.stop_possible());
  VERIFY(!ssrc.stop_requested());
  VERIFY(stok.stop_possible());
  VERIFY(!stok.stop_requested());

  // register callback
  bool cb1called{false};
  auto cb1 = [&]{
               std::cout << "cb1" << std::endl;
               cb1called = true;
             };
  {
    std::stop_callback scb1{stok, cb1};
    VERIFY(ssrc.stop_possible());
    VERIFY(!ssrc.stop_requested());
    VERIFY(stok.stop_possible());
    VERIFY(!stok.stop_requested());
    VERIFY(!cb1called);
  } // unregister callback

  // register another callback
  bool cb2called{false};
  auto cb2 = [&]{
               VERIFY(stok.stop_requested());
               cb2called = true;
             };
  std::stop_callback scb2a{stok, cb2}; // copies cb2
  //  std::stop_callback scb2b{stok, std::move(cb2)};
  VERIFY(ssrc.stop_possible());
  VERIFY(!ssrc.stop_requested());
  VERIFY(stok.stop_possible());
  VERIFY(!stok.stop_requested());
  VERIFY(!cb1called);
  VERIFY(!cb2called);

  // request stop
  auto b = ssrc.request_stop();
  VERIFY(b);
  VERIFY(ssrc.stop_possible());
  VERIFY(ssrc.stop_requested());
  VERIFY(stok.stop_possible());
  VERIFY(stok.stop_requested());
  VERIFY(!cb1called);
  VERIFY(cb2called);

  b = ssrc.request_stop();
  VERIFY(!b);

  // TODO verify the standard requires this
#if 0
  // register another callback
  bool cb3called{false};
  std::stop_callback scb3{stok, [&]
                                {
                                  cb3called = true;
                                }};
  VERIFY(ssrc.stop_possible());
  VERIFY(ssrc.stop_requested());
  VERIFY(stok.stop_possible());
  VERIFY(stok.stop_requested());
  VERIFY(!cb1called);
  VERIFY(cb2called);
  VERIFY(cb3called);
#endif
}
