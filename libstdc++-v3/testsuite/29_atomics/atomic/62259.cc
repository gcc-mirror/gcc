// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
// { dg-require-atomic-builtins "" }
// { dg-require-cstdint "" }

#include <atomic>
#include <cstdint>

using std::int32_t;
using std::int64_t;

// libstdc++/62259

struct twoints {
  int32_t a;
  int32_t b;
};

static_assert( alignof(std::atomic<twoints>) >= alignof(int64_t),
               "std::atomic not suitably aligned" );

// libstdc++/65147

struct power_of_two_obj {
    char c [8];
};

std::atomic<power_of_two_obj> obj1;

static_assert( __alignof__(obj1) >= alignof(int64_t),
               "std::atomic not suitably aligned" );

struct container_struct {
   char c[1];
   std::atomic<power_of_two_obj> ao;
};

container_struct obj2;

static_assert( __alignof__(obj2.ao) >= alignof(int64_t),
               "std::atomic not suitably aligned" );
