// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-add-options libatomic }
// { dg-do run { target c++20 } }

#include <stop_token>
#include <memory>
#include <testsuite_hooks.h>

void
test01()
{
  std::stop_source ssrc;
  std::stop_token stok = ssrc.get_token();
  using F = void(*)();
  std::unique_ptr<std::stop_callback<F>> pcb;
  auto dereg = [&pcb] { pcb.reset(); };
  std::stop_callback cb1(stok, dereg);
  pcb = std::make_unique<std::stop_callback<F>>(stok, []{});
  std::stop_callback cb2(stok, dereg);

  // PR libstdc++/92895
  // Making a stop request runs the callbacks. Whichever of cb1 and cb2
  // runs first will destroy *pcb, which will try to unregister it.
  // This recursive access to the shared stop state within a callback must
  // work without deadlock.
  ssrc.request_stop();
}

int main()
{
  test01();
}
