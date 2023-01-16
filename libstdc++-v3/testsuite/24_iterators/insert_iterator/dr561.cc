// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

// Example from https://wg21.link/lwg561

#include <iterator>
#include <string>

namespace my
{

template <class String>
struct my_type {};

struct my_container
{
template <class String>
void push_back(const my_type<String>&);
};

template <class String>
void inserter(const my_type<String>& m, my_container& c) {c.push_back(m);}

}  // my

int main()
{
    my::my_container c;
    my::my_type<std::string> m;
    inserter(m, c);
}
