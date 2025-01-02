// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }
// FIXME [!HOSTED]: avoidable std::allocator use
// { dg-require-effective-target hosted }

#include <memory>
#include <scoped_allocator>

// DR 2586. Wrong value category used in scoped_allocator_adaptor::construct()

struct X {
  using allocator_type = std::allocator<X>;
  X(std::allocator_arg_t, allocator_type&&) { }
  X(const allocator_type&) { }
};

int main() {
  std::scoped_allocator_adaptor<std::allocator<X>> sa;
  sa.construct(sa.allocate(1));
}
