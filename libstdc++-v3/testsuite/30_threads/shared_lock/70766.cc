// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <shared_mutex>

namespace adl
{
  template<typename T>
    void operator&(const T&) = delete;

  struct M : std::shared_timed_mutex { };
}

void
test01()
{
  using namespace std::chrono_literals;

  adl::M m;
  std::shared_lock<adl::M> l1(m);
  std::shared_lock<adl::M> l2(m, std::defer_lock);
  std::shared_lock<adl::M> l3(m, std::try_to_lock);
  m.lock_shared();
  std::shared_lock<adl::M> l4(m, std::adopt_lock);
  std::shared_lock<adl::M> l5(m, std::chrono::system_clock::now() + 1ms);
  std::shared_lock<adl::M> l6(m, 1ms);
}
