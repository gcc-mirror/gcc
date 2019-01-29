// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }
// { dg-require-sleep "" }

// Copyright (C) 2012-2019 Free Software Foundation, Inc.
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

#include <chrono>
#include <thread>
#include <future>
#include <set>
#include <testsuite_hooks.h>

struct Task;

std::set<const Task*> dead_tasks;

struct Task
{
  ~Task() { dead_tasks.insert(this); }

  void operator()() const
  {
    std::this_thread::sleep_for(std::chrono::seconds(1));
    VERIFY( dead_tasks.count(this) == 0 );
  }
};

int main()
{
  (void) std::async(std::launch::async, Task());
}
