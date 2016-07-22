// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }

#include <any>
#include <testsuite_hooks.h>

struct LocationAware
{
  LocationAware() { }
  ~LocationAware() { VERIFY(self == this); }
  LocationAware(const LocationAware&) { }
  LocationAware& operator=(const LocationAware&) { return *this; }
  LocationAware(LocationAware&&) noexcept { }
  LocationAware& operator=(LocationAware&&) noexcept { return *this; }

  void* const self = this;
};
static_assert(std::is_nothrow_move_constructible<LocationAware>::value, "");
static_assert(!std::is_trivially_copyable<LocationAware>::value, "");

using std::any;

void
test01()
{

  LocationAware l;
  any a = l;
}

void
test02()
{
  LocationAware l;
  any a = l;
  any b = a;
  {
    any tmp = std::move(a);
    a = std::move(b);
    b = std::move(tmp);
  }
}

void
test03()
{
  LocationAware l;
  any a = l;
  any b = a;
  swap(a, b);
}

int
main()
{
  test01();
  test02();
  test03();
}
