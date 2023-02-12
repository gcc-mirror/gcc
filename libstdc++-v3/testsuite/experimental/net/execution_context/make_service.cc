// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/executor>

namespace net = std::experimental::net;

struct S : net::execution_context::service
{
  using key_type = S;

  S(net::execution_context&);

  void shutdown() noexcept override { }
};

void test01(net::execution_context& c)
{
  net::make_service<S>(c);
}

static_assert(std::is_default_constructible<net::service_already_exists>(),
	      "LWG 3414. service_already_exists has no usable constructors");
