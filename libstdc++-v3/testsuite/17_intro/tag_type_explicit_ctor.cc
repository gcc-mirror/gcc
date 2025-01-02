// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <new>
#include <utility>
#include <memory>

#if __STDC_HOSTED__
#  include <mutex>
#endif

void f1(std::nothrow_t);
void f2(std::piecewise_construct_t);
void f3(std::allocator_arg_t);
#if __STDC_HOSTED__
void f4(std::defer_lock_t);
void f5(std::try_to_lock_t);
void f6(std::adopt_lock_t);
#endif

#pragma GCC diagnostic ignored "-Wunused-variable"

int main()
{
  std::nothrow_t v1;
  std::piecewise_construct_t v2;
  std::allocator_arg_t v3;
#if __STDC_HOSTED__
  std::defer_lock_t v4;
  std::try_to_lock_t v5;
  std::try_to_lock_t v6;
#endif
  std::nothrow_t v7 = {}; // { dg-error "explicit" }
  std::piecewise_construct_t v8 = {}; // { dg-error "explicit" }
  std::allocator_arg_t v9 = {}; // { dg-error "explicit" }
#if __STDC_HOSTED__
  std::defer_lock_t v10 = {};  // { dg-error "explicit" "" { target hosted } }
  std::try_to_lock_t v11 = {}; // { dg-error "explicit" "" { target hosted } }
  std::try_to_lock_t v12 = {}; // { dg-error "explicit" "" { target hosted } }
#endif
  f1(std::nothrow_t{});
  f2(std::piecewise_construct_t{});
  f3(std::allocator_arg_t{});
  f1({}); // { dg-error "explicit" }
  f2({}); // { dg-error "explicit" }
  f3({}); // { dg-error "explicit" }
#if __STDC_HOSTED__
  f4(std::defer_lock_t{});
  f5(std::try_to_lock_t{});
  f6(std::adopt_lock_t{});
  f4({}); // { dg-error "explicit" "" { target hosted } }
  f5({}); // { dg-error "explicit" "" { target hosted } }
  f6({}); // { dg-error "explicit" "" { target hosted } }
#endif
}
