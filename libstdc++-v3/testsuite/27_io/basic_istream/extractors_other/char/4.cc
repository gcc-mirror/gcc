// { dg-do run { target c++11 } }

// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// C++11 27.7.2.6 Rvalue stream extraction [istream.rvalue]

#include <sstream>

template<typename Istream, typename T, typename = void>
  struct is_extractable : std::false_type
  { };

template<typename> using void_t = void;

template<typename Istream, typename T>
  using extract_result
    = decltype(std::declval<Istream>() >> std::declval<const T&>());

template<typename Istream, typename T>
  struct is_extractable<Istream, T, void_t<extract_result<Istream, T>>>
  : std::true_type
  { };

struct X {};
std::istream& operator>>(std::istream&, X&) = delete;

struct Y {};
std::istream& operator>>(std::istream& is, Y&) {return is;}
std::istream& operator>>(std::istream& is, Y&&) {return is;}

struct Z{};

void test01()
{
  Y y;
  std::istringstream is;
  is >> y;
  is >> Y();
  std::istringstream() >> y;
  std::istringstream() >> Y();
  static_assert(!is_extractable<std::istream&, X&>::value, "");
  static_assert(!is_extractable<std::istream&&, X&>::value, "");
  static_assert(!is_extractable<std::istream&, X&&>::value, "");
  static_assert(!is_extractable<std::istream&&, X&&>::value, "");
  static_assert(is_extractable<std::istream&, Y&>::value, "");
  static_assert(is_extractable<std::istream&&, Y&>::value, "");
  static_assert(is_extractable<std::istream&, Y&&>::value, "");
  static_assert(is_extractable<std::istream&&, Y&&>::value, "");
  static_assert(!is_extractable<std::istream&, Z&>::value, "");
  static_assert(!is_extractable<std::istream&&, Z&>::value, "");
  static_assert(!is_extractable<std::istream&, Z&&>::value, "");
  static_assert(!is_extractable<std::istream&&, Z&&>::value, "");
}

int main()
{
  test01();
}
