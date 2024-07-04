// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

#include <atomic>

namespace adl
{
  template<typename T>
    void operator&(const T&) = delete;

  struct X {
    int i;
  };
}

void
test01()
{
  adl::X x;
  std::atomic<adl::X> a;
  a.store(x);
  x = a.load();
  x = a.exchange(x);
  a.compare_exchange_weak(x, x);
  a.compare_exchange_strong(x, x);
}

void
test02()
{
  adl::X x;
  volatile std::atomic<adl::X> a;
  a.store(x);
  x = a.load();
  x = a.exchange(x);
  a.compare_exchange_weak(x, x);
  a.compare_exchange_strong(x, x);
}
