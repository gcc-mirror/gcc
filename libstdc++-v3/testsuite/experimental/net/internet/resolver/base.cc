// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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
// { dg-require-effective-target net_ts_ip }
// { dg-add-options net_ts }

#include <experimental/internet>
#include <testsuite_common_types.h>
#include <testsuite_hooks.h>

using std::experimental::net::ip::resolver_base;

static_assert( __gnu_test::test_bitmask_values({
  resolver_base::passive,
  resolver_base::canonical_name,
  resolver_base::numeric_host,
#ifdef AI_NUMERICSERV
  resolver_base::numeric_service,
#endif
#ifdef AI_V4MAPPED
  resolver_base::v4_mapped,
#endif
#ifdef AI_ALL
  resolver_base::all_matching,
#endif
#ifdef AI_ADDRCONFIG
  resolver_base::address_configured
#endif
}), "each bitmask element is distinct" );

static_assert( ! std::is_default_constructible<resolver_base>(), "protected" );
static_assert( ! std::is_destructible<resolver_base>(), "protected" );

struct Res : resolver_base { };
static_assert( std::is_default_constructible<Res>(), "" );
static_assert( std::is_destructible<Res>(), "" );
