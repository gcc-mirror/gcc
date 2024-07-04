// { dg-do run { target c++20 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

template<typename S>
  void
  test (S va, S vb)
  {
    if constexpr (std::atomic_ref<S>::is_always_lock_free)
    {
      S aa{ va };
      std::atomic_ref<S> a{ aa };
      a.wait(vb);
      std::thread t([&]
        {
	  a.store(vb);
	  a.notify_one();
        });
      a.wait(va);
      t.join();
    }
  }

int
main ()
{
  test<int>(0, 42);
  test<long>(0, 42);
  test<unsigned>(0u, 42u);
  test<float>(0.0f, 42.0f);
  test<double>(0.0, 42.0);
  test<void*>(nullptr, reinterpret_cast<void*>(42));

  struct S{ int i; };
  test<S>(S{ 0 }, S{ 42 });
  return 0;
}
