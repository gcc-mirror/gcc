// { dg-do compile { target c++11 } }

// Copyright (C) 2016-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <utility>

#include <type_traits>
#include <memory>

using namespace std;

struct Poison
{
	Poison(Poison&&) = delete;
};

struct ThrowingCopy
{
  ThrowingCopy(const ThrowingCopy&);
  ThrowingCopy& operator=(const ThrowingCopy&);
};

int main()
{
	static_assert(!is_copy_constructible<Poison>::value, "");
	static_assert(!is_move_constructible<Poison>::value, "");
	static_assert(!is_copy_assignable<Poison>::value, "");
	static_assert(!is_move_assignable<Poison>::value, "");
	static_assert(!is_copy_constructible<std::pair<int, Poison>>::value,
		      "");
	static_assert(!is_move_constructible<std::pair<int, Poison>>::value,
		      "");
	static_assert(!is_copy_assignable<std::pair<int, Poison>>::value, "");
	static_assert(!is_move_assignable<std::pair<int, Poison>>::value, "");
	static_assert(!is_constructible<std::pair<int, Poison>&,
		      std::pair<char, Poison>&>::value, "");
	static_assert(!is_assignable<std::pair<int, Poison>&,
		      std::pair<char, Poison>&>::value, "");
	static_assert(!is_constructible<std::pair<int, Poison>&,
		      std::pair<char, Poison>>::value, "");
	static_assert(!is_assignable<std::pair<int, Poison>&,
		      std::pair<char, Poison>>::value, "");
	static_assert(!is_copy_constructible<std::pair<ThrowingCopy,
		      std::unique_ptr<int>>>::value,
		      "");
	static_assert(is_move_constructible<std::pair<ThrowingCopy,
		      std::unique_ptr<int>>>::value,
		      "");
	static_assert(!is_nothrow_move_constructible<std::pair<ThrowingCopy,
		      std::unique_ptr<int>>>::value,
		      "");
	static_assert(!is_copy_assignable<std::pair<ThrowingCopy,
		      std::unique_ptr<int>>>::value,
		      "");
	static_assert(is_move_assignable<std::pair<ThrowingCopy,
		      std::unique_ptr<int>>>::value,
		      "");
	static_assert(!is_nothrow_move_assignable<std::pair<ThrowingCopy,
		      std::unique_ptr<int>>>::value,
		      "");
}
