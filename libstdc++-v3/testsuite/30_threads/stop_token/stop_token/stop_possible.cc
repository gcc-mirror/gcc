// Copyright (C) 2020 Free Software Foundation, Inc.
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
#include <memory>
#include <testsuite_hooks.h>

void
test01()
{
  std::stop_source ssrc;
  std::stop_token tok = ssrc.get_token();
  VERIFY(tok.stop_possible());

  ssrc.request_stop();
  VERIFY(tok.stop_possible());
}

void
test02()
{
  std::stop_token tok = std::stop_source().get_token();
  // PR libstdc++/92895
  // stop_possible() is false when there is no associated stop_source
  VERIFY(!tok.stop_possible());
}

int main()
{
  test01();
  test02();
}
