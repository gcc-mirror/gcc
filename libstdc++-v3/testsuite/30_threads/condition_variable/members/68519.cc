// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

#include <condition_variable>
#include <testsuite_hooks.h>

// PR libstdc++/68519

bool val = false;
std::mutex mx;
std::condition_variable cv;

void
test01()
{
  for (int i = 0; i < 3; ++i)
  {
    std::unique_lock<std::mutex> l(mx);
    auto start = std::chrono::system_clock::now();
    cv.wait_for(l, std::chrono::duration<float>(1), [] { return val; });
    auto t = std::chrono::system_clock::now();
    VERIFY( (t - start) >= std::chrono::seconds(1) );
  }
}

int
main()
{
  test01();
}
