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

#include <experimental/executor>
#include <testsuite_hooks.h>

using std::experimental::net::execution_context;
using std::experimental::net::use_service;

struct service1 : execution_context::service
{
  using key_type = service1;
  service1(execution_context& c) : service(c) { }
  void shutdown() noexcept { }
};

struct key2 : execution_context::service
{
  key2(execution_context& c) : service(c) { }
};

struct service2 : key2
{
  using key_type = key2;
  service2(execution_context& c) : key2(c) { }
  void shutdown() noexcept { }
};

struct service3 : service1
{
  using service1::service1;
};

struct service4 : service2
{
  using service2::service2;
};

void
test01()
{
  execution_context ctx;
  service1& svc1 = use_service<service1>(ctx);
  service1& svc1a = use_service<service1>(ctx);
  VERIFY( &svc1a == &svc1 );

  key2& svc2 = use_service<service2>(ctx);
  key2& svc2a = use_service<service2>(ctx);
  VERIFY( &svc2a == &svc2 );

  service1& svc3 = use_service<service3>(ctx);
  VERIFY( &svc3 == &svc1 );

  key2& svc4 = use_service<service4>(ctx);
  VERIFY( &svc4 == &svc2 );

  // TODO test02() function that puts derived types in first, then tests base comes out
}

int
main()
{
  test01();
}
