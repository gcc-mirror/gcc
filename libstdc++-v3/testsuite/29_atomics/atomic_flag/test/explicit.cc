// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-thread-fence "" }

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

int main()
{
  using namespace std;

  atomic_flag af0 = ATOMIC_FLAG_INIT;
  VERIFY( ! af0.test(memory_order_acquire) );

  atomic_flag af{true};
  const atomic_flag& caf = af;

  VERIFY( af.test(memory_order_acquire) );
  VERIFY( caf.test(memory_order_acquire) );
  af.clear(memory_order_release);
  VERIFY( ! af.test(memory_order_acquire) );
  VERIFY( ! caf.test(memory_order_acquire) );
}
