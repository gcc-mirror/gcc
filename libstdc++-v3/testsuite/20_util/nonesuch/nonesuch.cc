// 2019-05-14  Nina Dinka Ranns  <dinka.ranns@gmail.com>
// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <type_traits>
// Example taken from LWG2960

using std::__nonesuch;
struct such {};
void f(const such&){};
void f(const std::__nonesuch&);

int main(){
 static_assert(!std::is_default_constructible<__nonesuch>::value,
		 "__nonesuch is default constructible");
 static_assert(!std::is_copy_constructible<__nonesuch>::value,
		 "__nonesuch is copy constructible");
 static_assert(!std::is_assignable<__nonesuch, __nonesuch>::value,
		 "__nonesuch is assignable");
 static_assert(!std::is_destructible<__nonesuch>::value,
		 "__nonesuch is destructible");
 f({});
}
