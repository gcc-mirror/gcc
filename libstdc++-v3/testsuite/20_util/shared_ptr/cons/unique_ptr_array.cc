// { dg-do run { target c++11 } }

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

// 20.7.2.2 Class template shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>

#if __cpp_lib_shared_ptr_arrays >= 201603
# define SHARED_PTR_ARRAYS
#endif
#if __cpp_lib_enable_shared_from_this >= 201603
# define WEAK_FROM_THIS
#endif

int destroyed = 0;

struct A : std::enable_shared_from_this<A>
{
  ~A() { ++destroyed; }
};

// 20.7.2.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Construction from unique_ptr<A[]>
int
test01()
{
  std::unique_ptr<A[]> up(new A[2]);
#ifdef SHARED_PTR_ARRAYS
  std::shared_ptr<A[]> sp(std::move(up));
#else
  std::shared_ptr<A> sp(std::move(up));
#endif
  VERIFY( up.get() == 0 );
  VERIFY( sp.get() != 0 );
  VERIFY( sp.use_count() == 1 );

#ifdef SHARED_PTR_ARRAYS
# ifdef WEAK_FROM_THIS
  VERIFY( sp[0].weak_from_this().expired() );
# endif
#else
  VERIFY( sp->shared_from_this() != nullptr );
#endif

  sp.reset();
  VERIFY( destroyed == 2 );

  return 0;
}

int
main()
{
  test01();
  return 0;
}
