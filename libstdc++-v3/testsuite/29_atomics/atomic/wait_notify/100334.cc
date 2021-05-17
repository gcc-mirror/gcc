// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-add-options libatomic }

// Copyright (C) 2021 Free Software Foundation, Inc.
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
#include <future>

#include <testsuite_hooks.h>

template <typename T>
struct atomics_sharing_same_waiter
{
   std::atomic<T> tmp[49 * 4] = {};
   std::atomic<T>* a[4] = {
      { &tmp[0] },
      { &tmp[16 * 4] },
      { &tmp[32 * 4] },
      { &tmp[48 * 4] }
   };
};

constexpr unsigned key(void * a)
{
  constexpr uintptr_t ct = 16;
  return (uintptr_t(a) >> 2) % ct;
}

int
main()
{
  // all atomic share the same waiter
//  atomics_sharing_same_waiter<char> atomics;
  atomics_sharing_same_waiter<char> atomics;
  for (auto& atom : atomics.a)
  {
    atom->store(0);
  }

  auto a = &std::__detail::__waiter_pool_base::_S_for(reinterpret_cast<char *>(atomics.a[0]));
  auto b = &std::__detail::__waiter_pool_base::_S_for(reinterpret_cast<char *>(atomics.a[1]));
  VERIFY( a == b );

  auto fut0 = std::async(std::launch::async, [&] { atomics.a[0]->wait(0); });
  auto fut1 = std::async(std::launch::async, [&] { atomics.a[1]->wait(0); });
  auto fut2 = std::async(std::launch::async, [&] { atomics.a[2]->wait(0); });
  auto fut3 = std::async(std::launch::async, [&] { atomics.a[3]->wait(0); });

  // make sure the all threads already await
  std::this_thread::sleep_for(std::chrono::milliseconds{100});

  atomics.a[2]->store(1);
  atomics.a[2]->notify_one();

  VERIFY(std::future_status::timeout == fut0.wait_for(std::chrono::milliseconds{100}));
  VERIFY(atomics.a[0]->load() == 0);

  VERIFY(std::future_status::timeout == fut1.wait_for(std::chrono::milliseconds{100}));
  VERIFY(atomics.a[1]->load() == 0);

  VERIFY(std::future_status::ready == fut2.wait_for(std::chrono::milliseconds{100}));
  VERIFY(atomics.a[2]->load() == 1);

  VERIFY(std::future_status::timeout == fut3.wait_for(std::chrono::milliseconds{100}));
  VERIFY(atomics.a[3]->load() == 0);

  atomics.a[0]->store(1);
  atomics.a[0]->notify_one();
  atomics.a[1]->store(1);
  atomics.a[1]->notify_one();
  atomics.a[3]->store(1);
  atomics.a[3]->notify_one();

  return 0;
}
