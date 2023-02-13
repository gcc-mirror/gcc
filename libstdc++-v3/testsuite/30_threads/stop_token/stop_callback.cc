// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
#include <functional>
#include <testsuite_hooks.h>

void
test01()
{
  bool called = false;
  std::function<void()> f = [&called]{ called = true; };
  std::stop_source ssrc;
  ssrc.request_stop();
  std::stop_token tok = ssrc.get_token();
  std::stop_callback cb1(tok, f);
  VERIFY( tok.stop_possible() );
  VERIFY( f != nullptr );
  VERIFY( called == true );

  called = false;
  std::stop_callback cb2(std::move(tok), f);
  // when callback is executed immediately, no change in ownership:
  VERIFY( tok.stop_possible() );
  VERIFY( f != nullptr );
  VERIFY( called == true );

  std::stop_token sink(std::move(tok)); // leave tok empty

  called = false;
  std::stop_callback cb3(tok, f);
  VERIFY( f != nullptr );
  VERIFY( called == false );

  called = false;
  std::stop_callback cb4(std::move(tok), f);
  VERIFY( f != nullptr );
  VERIFY( called == false );
}

void
test02()
{
  bool called = false;
  std::function<void()> f0 = [&called]{ called = true; };
  std::function<void()> f = f0;
  std::stop_source ssrc;
  ssrc.request_stop();
  std::stop_token tok = ssrc.get_token();

  std::stop_callback cb1(tok, std::move(f));
  VERIFY( tok.stop_possible() );
  VERIFY( f == nullptr );
  VERIFY( called == true );

  called = false;
  f = f0;
  std::stop_callback cb2(std::move(tok), std::move(f));
  // when callback is executed immediately, no change in ownership:
  VERIFY( tok.stop_possible() );
  VERIFY( f == nullptr );
  VERIFY( called == true );

  std::stop_token sink(std::move(tok)); // leave tok empty

  called = false;
  f = f0;
  std::stop_callback cb3(tok, std::move(f));
  VERIFY( f == nullptr );
  VERIFY( called == false );

  called = false;
  f = f0;
  std::stop_callback cb4(std::move(tok), std::move(f));
  VERIFY( f == nullptr );
  VERIFY( called == false );
}

void
test03()
{
  bool called[4] = { };
  std::stop_source ssrc;
  std::stop_token tok = ssrc.get_token();
  std::stop_callback cb1(tok, [&]{ called[0] = true; });
  VERIFY( tok.stop_possible() );
  VERIFY( called[0] == false );

  std::stop_callback cb2(std::move(tok), [&]{ called[1] = true; });
  VERIFY( !tok.stop_possible() );
  VERIFY( called[1] == false );

  std::stop_callback cb3(tok, [&]{ called[2] = true; });
  VERIFY( called[2] == false );

  std::stop_callback cb4(std::move(tok), [&]{ called[3] = true; });
  VERIFY( called[3] == false );

  ssrc.request_stop();
  VERIFY( called[0] == true );
  VERIFY( called[1] == true );
  VERIFY( called[2] == false );
  VERIFY( called[3] == false );
}

int main()
{
  test01();
  test02();
  test03();
}
