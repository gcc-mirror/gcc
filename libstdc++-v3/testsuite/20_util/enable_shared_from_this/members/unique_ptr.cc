// { dg-do run { target c++11 } }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <memory>
#include <ext/pointer.h>
#include <testsuite_hooks.h>

struct X : public std::enable_shared_from_this<X> { };

void test01()
{
  std::unique_ptr<X> up(new X);
  X* xp = up.get();
  std::shared_ptr<X> sp(std::move(up));
  VERIFY( xp->shared_from_this() != nullptr );
}

using __gnu_cxx::_Pointer_adapter;
using __gnu_cxx::_Std_pointer_impl;

struct Deleter
{
  struct pointer : _Pointer_adapter<_Std_pointer_impl<X>>
  {
    using _Pointer_adapter::_Pointer_adapter;
    operator X*() const noexcept { return this->get(); }
  };

  void operator()(pointer p) const noexcept { delete (X*)p; }
};

void test02()
{
  std::unique_ptr<X, Deleter> up(new X);
  Deleter::pointer xp = up.get();
  // Creating shared_ptr from unique_ptr with custom pointer is an extension:
  std::shared_ptr<X> sp(std::move(up));
  // but enable_shared_from_this should still work:
  VERIFY( xp->shared_from_this() != nullptr );
}

int main()
{
  test01();
  test02();
}
