// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <random>
#include <memory>
#include <thread>
#include <cstdio>
#include <testsuite_random.h>

using __gnu_test::random_device_available;

void read_random_device(const char* token, int iterations)
{
  std::random_device dev(token);
    for (int i = 0; i != iterations; ++i)
      (void) dev();
}

int main() {
  std::thread workers[8];

  // N.B. don't test /dev/random as it might block, and /dev/urandom
  // "can incur an appreciable delay when requesting large amounts of data".
  for (const char* dev : { "default", "rdrand", "rdseed", "rand_s" })
  {
    if (random_device_available(dev))
    {
      for (auto& w : workers)
	w = std::thread{read_random_device, dev, 1000};
      for (auto& w : workers)
	w.join();
    }
    else
      std::printf("random_device(\"%s\") not available\n", dev);
  }
}
