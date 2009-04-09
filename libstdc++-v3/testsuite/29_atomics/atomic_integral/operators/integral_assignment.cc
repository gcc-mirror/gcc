// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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


#include <cstdatomic>
#include <testsuite_hooks.h>
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::integral_assignable test;
  __gnu_cxx::typelist::apply_generator(test,
				       __gnu_test::atomic_integrals::type(), 
				       __gnu_test::integral_types::type());
  return 0;
}
