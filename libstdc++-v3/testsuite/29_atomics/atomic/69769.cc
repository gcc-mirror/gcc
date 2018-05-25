// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }
// { dg-require-atomic-builtins "" }

#include <atomic>

void
test01()
{
  std::atomic<void*> p;
  p.fetch_add(1); // { dg-error "from here" }
  p.fetch_sub(1); // { dg-error "from here" }
  p += 1;	  // { dg-error "from here" }
  p -= 1;	  // { dg-error "from here" }
  ++p;		  // { dg-error "from here" }
  p++;		  // { dg-error "from here" }
  --p;		  // { dg-error "from here" }
  p--;		  // { dg-error "from here" }
}

void
test02()
{
  std::atomic<void(*)()> p;
  p.fetch_add(1); // { dg-error "from here" }
  p.fetch_sub(1); // { dg-error "from here" }
  p += 1;	  // { dg-error "from here" }
  p -= 1;	  // { dg-error "from here" }
  ++p;		  // { dg-error "from here" }
  p++;		  // { dg-error "from here" }
  --p;		  // { dg-error "from here" }
  p--;		  // { dg-error "from here" }
}

void
test03()
{
  volatile std::atomic<void*> p;
  p.fetch_add(1); // { dg-error "from here" }
  p.fetch_sub(1); // { dg-error "from here" }
  p += 1;	  // { dg-error "from here" }
  p -= 1;	  // { dg-error "from here" }
  ++p;		  // { dg-error "from here" }
  p++;		  // { dg-error "from here" }
  --p;		  // { dg-error "from here" }
  p--;		  // { dg-error "from here" }
}

void
test04()
{
  volatile std::atomic<void(*)()> p;
  p.fetch_add(1); // { dg-error "from here" }
  p.fetch_sub(1); // { dg-error "from here" }
  p += 1;	  // { dg-error "from here" }
  p -= 1;	  // { dg-error "from here" }
  ++p;		  // { dg-error "from here" }
  p++;		  // { dg-error "from here" }
  --p;		  // { dg-error "from here" }
  p--;		  // { dg-error "from here" }
}

// { dg-prune-output "static assertion failed" }
