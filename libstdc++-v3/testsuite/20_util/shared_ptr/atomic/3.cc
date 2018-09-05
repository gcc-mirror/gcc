// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#include <thread>
#include <memory>
#include <testsuite_hooks.h>

struct leaddock
{
  ~leaddock();
};

std::shared_ptr<leaddock> global;

leaddock::~leaddock()
{
  // If this destructor is called "inside" an atomic operation on global it
  // will deadlock, so this checks that the atomic_store is done atomically.
  auto copy = std::atomic_load(&global);
  VERIFY( !copy );
}

void f()
{
  std::atomic_store(&global, std::make_shared<leaddock>());
  std::atomic_store(&global, {});
}

int main()
{
  std::thread{ f }.join();
}
