// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }
// { dg-additional-options "-pthread" { target pthread } }

// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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


#include <atomic>
#include <thread>

#include <testsuite_hooks.h>

template<typename Tp>
  void
  check()
  {
    std::atomic<Tp> a{ Tp(1) };
    VERIFY( a.load() == Tp(1) );
    a.wait( Tp(0) );
    std::thread t([&]
      {
        a.store(Tp(0));
        a.notify_one();
      });
    a.wait(Tp(1));
    t.join();
  }

int
main ()
{
  // check<bool> bb;
  check<char>();
  check<signed char>();
  check<unsigned char>();
  check<short>();
  check<unsigned short>();
  check<int>();
  check<unsigned int>();
  check<long>();
  check<unsigned long>();
  check<long long>();
  check<unsigned long long>();

  check<wchar_t>();
  check<char8_t>();
  check<char16_t>();
  check<char32_t>();

  check<int8_t>();
  check<int16_t>();
  check<int32_t>();
  check<int64_t>();

  check<uint8_t>();
  check<uint16_t>();
  check<uint32_t>();
  check<uint64_t>();
  return 0;
}
