// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

#include <memory_resource>
#include <testsuite_hooks.h>

bool eq(const std::pmr::pool_options& lhs, const std::pmr::pool_options& rhs)
{
  return lhs.max_blocks_per_chunk == rhs.max_blocks_per_chunk
    && lhs.largest_required_pool_block == rhs.largest_required_pool_block;
}

void
test01()
{
  std::pmr::synchronized_pool_resource r0;
  const std::pmr::pool_options opts = r0.options();
  VERIFY( opts.max_blocks_per_chunk != 0 );
  VERIFY( opts.largest_required_pool_block != 0 );

  std::pmr::synchronized_pool_resource r1(opts);
  const auto opts1 = r1.options();
  VERIFY( eq(opts, opts1) );

  std::pmr::synchronized_pool_resource r2(std::pmr::pool_options{0, 0});
  const auto opts2 = r2.options();
  VERIFY( eq(opts, opts2) );
}

int
main()
{
  test01();
}
