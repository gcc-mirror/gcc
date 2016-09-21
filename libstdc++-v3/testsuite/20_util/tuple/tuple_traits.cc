// { dg-do compile { target c++11 } }

// Copyright (C) 2016 Free Software Foundation, Inc.
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

#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>
#include <memory>

using namespace std;

struct Poison
{
	Poison(Poison&&) = delete;
};


int main()
{
	static_assert(!is_copy_constructible<Poison>::value, "");
	static_assert(!is_move_constructible<Poison>::value, "");
	static_assert(!is_copy_assignable<Poison>::value, "");
	static_assert(!is_move_assignable<Poison>::value, "");

	static_assert(!is_copy_constructible<std::tuple<Poison>>::value, "");
	static_assert(!is_move_constructible<std::tuple<Poison>>::value, "");
	static_assert(!is_copy_assignable<std::tuple<Poison>>::value, "");
	static_assert(!is_move_assignable<std::tuple<Poison>>::value, "");

	static_assert(!is_copy_constructible<std::tuple<int, Poison>>::value,
		      "");
	static_assert(!is_move_constructible<std::tuple<int, Poison>>::value,
		      "");
	static_assert(!is_copy_assignable<std::tuple<int, Poison>>::value, "");
	static_assert(!is_move_assignable<std::tuple<int, Poison>>::value, "");
	static_assert(!is_constructible<std::tuple<int, Poison>&,
		      std::tuple<char, Poison>&>::value, "");
	static_assert(!is_assignable<std::tuple<int, Poison>&,
		      std::tuple<char, Poison>&>::value, "");
	static_assert(!is_constructible<std::tuple<int, Poison>&,
		      std::tuple<char, Poison>>::value, "");
	static_assert(!is_assignable<std::tuple<int, Poison>&,
		      std::tuple<char, Poison>>::value, "");
	static_assert(!is_constructible<std::tuple<int, Poison>&,
		      std::pair<char, Poison>&>::value, "");
	static_assert(!is_assignable<std::tuple<int, Poison>&,
		      std::pair<char, Poison>&>::value, "");
	static_assert(!is_constructible<std::tuple<int, Poison>&,
		      std::pair<char, Poison>>::value, "");
	static_assert(!is_assignable<std::tuple<int, Poison>&,
		      std::pair<char, Poison>>::value, "");

	static_assert(!is_copy_constructible<
		      std::tuple<int, int, Poison>>::value, "");
	static_assert(!is_move_constructible<
		      std::tuple<int, int, Poison>>::value, "");
	static_assert(!is_copy_assignable<
		      std::tuple<int, int, Poison>>::value, "");
	static_assert(!is_move_assignable<
		      std::tuple<int, int, Poison>>::value, "");
	static_assert(!is_constructible<
		      std::tuple<int, int,Poison>&,
		      std::tuple<int, char, Poison>&>::value, "");
	static_assert(!is_assignable<
		      std::tuple<int, int, Poison>&,
		      std::tuple<int, char, Poison>&>::value, "");
	static_assert(!is_constructible<
		      std::tuple<int, int, Poison>&,
		      std::tuple<int, char, Poison>>::value, "");
	static_assert(!is_assignable<
		      std::tuple<int, int, Poison>&,
		      std::tuple<int, char, Poison>>::value, "");
	static_assert(!is_constructible<
		      std::tuple<int, int, Poison>&,
		      std::pair<char, Poison>&>::value, "");
	static_assert(!is_assignable<
		      std::tuple<int, int, Poison>&,
		      std::pair<char, Poison>&>::value, "");
	static_assert(!is_constructible<
		      std::tuple<int, int, Poison>&,
		      std::pair<char, Poison>>::value, "");
	static_assert(!is_assignable<
		      std::tuple<int, int, Poison>&,
		      std::pair<char, Poison>>::value, "");

	static_assert(is_trivially_copy_constructible<tuple<int>>::value, "");
	static_assert(!is_trivially_move_constructible<tuple<int>>::value, "");

	static_assert(!is_trivially_copy_assignable<tuple<int>>::value, "");
	static_assert(!is_trivially_move_assignable<tuple<int>>::value, "");

	static_assert(is_copy_constructible<tuple<int>>::value, "");
	static_assert(is_move_constructible<tuple<int>>::value, "");

	static_assert(is_copy_assignable<tuple<int>>::value, "");
	static_assert(is_move_assignable<tuple<int>>::value, "");

	static_assert(!is_trivially_copy_constructible<
		      tuple<vector<int>>>::value, "");
	static_assert(!is_trivially_move_constructible<
		      tuple<vector<int>>>::value, "");

	static_assert(!is_trivially_copy_assignable<
		      tuple<vector<int>>>::value, "");
	static_assert(!is_trivially_move_assignable<
		      tuple<vector<int>>>::value, "");

	static_assert(is_copy_constructible<tuple<vector<int>>>::value, "");
	static_assert(is_move_constructible<tuple<vector<int>>>::value, "");

	static_assert(is_copy_assignable<tuple<vector<int>>>::value, "");
	static_assert(is_move_assignable<tuple<vector<int>>>::value, "");

	static_assert(!is_trivially_copy_constructible<
		      tuple<unique_ptr<int>>>::value, "");
	static_assert(!is_trivially_move_constructible<
		      tuple<unique_ptr<int>>>::value, "");

	static_assert(!is_trivially_copy_assignable<
		      tuple<unique_ptr<int>>>::value, "");
	static_assert(!is_trivially_move_assignable<
		      tuple<unique_ptr<int>>>::value, "");
	static_assert(!is_copy_constructible<
		      tuple<unique_ptr<int>>>::value, "");
	static_assert(is_move_constructible<tuple<unique_ptr<int>>>::value, "");

	static_assert(!is_copy_assignable<tuple<unique_ptr<int>>>::value, "");
	static_assert(is_move_assignable<tuple<unique_ptr<int>>>::value, "");

	static_assert(is_trivially_copy_constructible<
		      tuple<int, int>>::value, "");
	static_assert(!is_trivially_move_constructible<
		      tuple<int, int>>::value, "");

	static_assert(!is_trivially_copy_assignable<
		      tuple<int, int>>::value, "");
	static_assert(!is_trivially_move_assignable<
		      tuple<int, int>>::value, "");

	static_assert(is_copy_constructible<tuple<int, int>>::value, "");
	static_assert(is_move_constructible<tuple<int, int>>::value, "");

	static_assert(is_copy_assignable<tuple<int, int>>::value, "");
	static_assert(is_move_assignable<tuple<int, int>>::value, "");
	static_assert(!is_trivially_copy_constructible<
		      tuple<int, vector<int>>>::value, "");
	static_assert(!is_trivially_move_constructible<
		      tuple<int, vector<int>>>::value, "");

	static_assert(!is_trivially_copy_assignable<
		      tuple<int, vector<int>>>::value, "");
	static_assert(!is_trivially_move_assignable<
		      tuple<int, vector<int>>>::value, "");

	static_assert(is_copy_constructible<
		      tuple<int, vector<int>>>::value, "");
	static_assert(is_move_constructible<
		      tuple<int, vector<int>>>::value, "");

	static_assert(is_copy_assignable<tuple<int, vector<int>>>::value, "");
	static_assert(is_move_assignable<tuple<int, vector<int>>>::value, "");

	static_assert(!is_trivially_copy_constructible<
		      tuple<int, unique_ptr<int>>>::value, "");
	static_assert(!is_trivially_move_constructible<
		      tuple<int, unique_ptr<int>>>::value, "");

	static_assert(!is_trivially_copy_assignable<
		      tuple<int, unique_ptr<int>>>::value, "");
	static_assert(!is_trivially_move_assignable<
		      tuple<int, unique_ptr<int>>>::value, "");

	static_assert(!is_copy_constructible<
		      tuple<int, unique_ptr<int>>>::value, "");
	static_assert(is_move_constructible<
		      tuple<int, unique_ptr<int>>>::value, "");

	static_assert(!is_copy_assignable<
		      tuple<int, unique_ptr<int>>>::value, "");
	static_assert(is_move_assignable<
		      tuple<int, unique_ptr<int>>>::value, "");

	static_assert(is_copy_constructible<tuple<int, int, int>>::value, "");
	static_assert(is_move_constructible<tuple<int, int, int>>::value, "");

	static_assert(is_copy_assignable<tuple<int, int, int>>::value, "");
	static_assert(is_move_assignable<tuple<int, int, int>>::value, "");

	static_assert(!is_trivially_copy_constructible<
		      tuple<int, int, vector<int>>>::value, "");
	static_assert(!is_trivially_move_constructible<
		      tuple<int, int, vector<int>>>::value, "");

	static_assert(!is_trivially_copy_assignable<
		      tuple<int, int, vector<int>>>::value, "");
	static_assert(!is_trivially_move_assignable<
		      tuple<int, int, vector<int>>>::value, "");

	static_assert(is_copy_constructible<
		      tuple<int, int, vector<int>>>::value, "");
	static_assert(is_move_constructible<
		      tuple<int, int, vector<int>>>::value, "");

	static_assert(is_copy_assignable<
		      tuple<int, int, vector<int>>>::value, "");
	static_assert(is_move_assignable<
		      tuple<int, int, vector<int>>>::value, "");

	static_assert(!is_trivially_copy_constructible<
		      tuple<int, int, unique_ptr<int>>>::value, "");
	static_assert(!is_trivially_move_constructible<
		      tuple<int, int, unique_ptr<int>>>::value, "");

	static_assert(!is_trivially_copy_assignable<
		      tuple<int, int, unique_ptr<int>>>::value, "");
	static_assert(!is_trivially_move_assignable<
		      tuple<int, int, unique_ptr<int>>>::value, "");

	static_assert(!is_copy_constructible<
		      tuple<int, int, unique_ptr<int>>>::value, "");
	static_assert(is_move_constructible<
		      tuple<int, int, unique_ptr<int>>>::value, "");

	static_assert(!is_copy_assignable<
		      tuple<int, int, unique_ptr<int>>>::value, "");
	static_assert(is_move_assignable<
		      tuple<int, int, unique_ptr<int>>>::value, "");
}
