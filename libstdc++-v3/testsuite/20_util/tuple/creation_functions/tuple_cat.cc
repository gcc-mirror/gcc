// { dg-do run { target c++11 } }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

// Tuple

#include <tuple>
#include <array>

static_assert(std::is_same<decltype(std::tuple_cat()),
	      std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype(std::tuple_cat
				    (std::declval<std::tuple<>>())),
              std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype(std::tuple_cat
				    (std::declval<std::tuple<>&>())),
              std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype(std::tuple_cat
				    (std::declval<const std::tuple<>>())),
              std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype(std::tuple_cat
				    (std::declval<const std::tuple<>&>())),
              std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype(std::tuple_cat
				    (std::declval<std::pair<int, bool>>())),
              std::tuple<int, bool>>::value, "Error");
static_assert(std::is_same<decltype(std::tuple_cat
				    (std::declval<std::pair<int, bool>&>())),
              std::tuple<int, bool>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat(std::declval<const std::pair<int, bool>>())),
              std::tuple<int, bool>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat(std::declval<const std::pair<int, bool>&>())),
              std::tuple<int, bool>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat(std::declval<std::array<int, 3>>())),
              std::tuple<int, int, int>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat(std::declval<std::array<int, 3>&>())),
              std::tuple<int, int, int>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat(std::declval<const std::array<int, 3>>())),
              std::tuple<int, int, int>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat(std::declval<const std::array<int, 3>&>())),
              std::tuple<int, int, int>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat
	       (std::declval<std::tuple<>>(), std::declval<std::tuple<>>())),
              std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat
	       (std::declval<std::tuple<>>(), std::declval<std::tuple<>>(),
		std::declval<std::tuple<>>())), std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat
	       (std::declval<std::tuple<>>(),
		std::declval<std::array<char, 0>>(),
		std::declval<std::array<int, 0>>(),
		std::declval<std::tuple<>>())), std::tuple<>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat
	       (std::declval<std::tuple<int>>(),
		std::declval<std::tuple<double>>())),
              std::tuple<int, double>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat
	       (std::declval<std::tuple<int>>(),
		std::declval<std::tuple<double>>(),
		std::declval<std::tuple<const long&>>())),
              std::tuple<int, double, const long&>>::value, "Error");
static_assert(std::is_same<decltype
	      (std::tuple_cat
	       (std::declval<std::array<wchar_t, 3>&>(),
		std::declval<std::tuple<double>>(),
		std::declval<std::tuple<>>(),
		std::declval<std::tuple<unsigned&>>(),
		std::declval<std::pair<bool, std::nullptr_t>>())),
              std::tuple<wchar_t, wchar_t, wchar_t,
	      double, unsigned&, bool, std::nullptr_t>
              >::value, "Error");

int main()
{
  std::tuple_cat();
  std::tuple_cat(std::tuple<>{ });
  std::tuple_cat(std::tuple<>{ }, std::tuple<>{ });
  std::array<int, 3> a3;
  std::tuple_cat(a3);
  std::pair<double, bool> pdb;
  std::tuple<unsigned, float, std::nullptr_t, void*> t;
  int i{ };
  double d{ };
  int* pi{ };
  std::tuple<int&, double&, int*&> to{i, d, pi};
  std::tuple_cat(pdb);
  std::tuple_cat(to);
  std::tuple_cat(to, to);
  std::tuple_cat(a3, pdb);
  std::tuple_cat(a3, pdb, t);
  std::tuple_cat(a3, pdb, t, a3);
  std::tuple_cat(a3, pdb, t, a3, pdb, t);

  static_assert(std::is_same<decltype
		(std::tuple_cat(a3, pdb, t, a3, pdb, t)),
                std::tuple<int, int, int, double, bool,
		unsigned, float, std::nullptr_t, void*,
		int, int, int, double, bool, unsigned,
		float, std::nullptr_t, void*>
                >::value, "Error");

  std::tuple_cat(std::tuple<int, char, void*>{}, to, a3,
		 std::tuple<>{}, std::pair<float,
		 std::nullptr_t>{}, pdb, to);
}
