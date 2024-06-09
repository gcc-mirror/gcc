// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void
test01()
{
  std::stop_source ssrc;
  VERIFY( ssrc.stop_possible() );
  VERIFY( !ssrc.stop_requested() );
  VERIFY( ssrc == ssrc );
  VERIFY( !(ssrc != ssrc) );

  std::stop_source copy(ssrc);
  VERIFY( copy.stop_possible() );
  VERIFY( !copy.stop_requested() );
  VERIFY( ssrc.stop_possible() );
  VERIFY( !ssrc.stop_requested() );
  VERIFY( copy == ssrc );
  VERIFY( !(copy != ssrc) );
  VERIFY( ssrc == ssrc );
  VERIFY( !(ssrc != ssrc) );

  std::stop_source move(std::move(ssrc));
  VERIFY( move.stop_possible() );
  VERIFY( !move.stop_requested() );
  VERIFY( copy.stop_possible() );
  VERIFY( !copy.stop_requested() );
  VERIFY( !ssrc.stop_possible() );
  VERIFY( !ssrc.stop_requested() );
  VERIFY( !(move == ssrc) );
  VERIFY( move != ssrc );
  VERIFY( ssrc == ssrc );
  VERIFY( !(ssrc != ssrc) );
  VERIFY( move == copy );
  VERIFY( !(move != copy) );
  VERIFY( !(copy == ssrc) );
  VERIFY( copy != ssrc );
}

void
test02()
{
  // stop_source(nostopstate_t) constructor is explicit:
  static_assert(!std::is_convertible_v<std::nostopstate_t, std::stop_source>);

  std::stop_source ssrc(std::nostopstate);
  VERIFY( !ssrc.stop_possible() );
  VERIFY( !ssrc.stop_requested() );
  VERIFY( ssrc == ssrc );
  VERIFY( !(ssrc != ssrc) );

  std::stop_source copy(ssrc);
  VERIFY( !copy.stop_possible() );
  VERIFY( !copy.stop_requested() );
  VERIFY( !ssrc.stop_possible() );
  VERIFY( !ssrc.stop_requested() );
  VERIFY( copy == ssrc );
  VERIFY( !(copy != ssrc) );
  VERIFY( ssrc == ssrc );
  VERIFY( !(ssrc != ssrc) );

  std::stop_source move(std::move(ssrc));
  VERIFY( !move.stop_possible() );
  VERIFY( !move.stop_requested() );
  VERIFY( !copy.stop_possible() );
  VERIFY( !copy.stop_requested() );
  VERIFY( !ssrc.stop_possible() );
  VERIFY( !ssrc.stop_requested() );
  VERIFY( move == ssrc );
  VERIFY( !(move != ssrc) );
  VERIFY( ssrc == ssrc );
  VERIFY( !(ssrc != ssrc) );
  VERIFY( move == copy );
  VERIFY( !(move != copy) );
  VERIFY( copy == ssrc );
  VERIFY( !(copy != ssrc) );
}

void
test03()
{
  std::stop_source s1;
  std::stop_source copy(s1);
  s1.request_stop();
  std::stop_source s2(std::nostopstate);
  VERIFY( s1 != s2 );

  s1.swap(s2);
  VERIFY( !s1.stop_possible() );
  VERIFY( !s1.stop_requested() );
  VERIFY( s2.stop_possible() );
  VERIFY( s2.stop_requested() );
  VERIFY( s1 != s2 );
  VERIFY( s2 == copy );

  swap(s1, s2);
  VERIFY( s1.stop_possible() );
  VERIFY( s1.stop_requested() );
  VERIFY( !s2.stop_possible() );
  VERIFY( !s2.stop_requested() );
  VERIFY( s1 == copy );
}

int main()
{
  test01();
  test02();
  test03();
}
