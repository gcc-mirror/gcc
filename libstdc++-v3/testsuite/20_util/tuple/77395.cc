// { dg-do compile { target c++11 } }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#include <tuple>
#include <utility>

struct derived;
struct base
{
    operator derived & () &;
    operator derived const & () const &;
    operator derived && () &&;
};

struct derived : base {};

base::operator derived & () & { return *static_cast<derived *>(this); }
base::operator derived const & () const & { return *static_cast<derived const *>(this); }
base::operator derived && () && { return std::move(*static_cast<derived *>(this)); }

std::tuple<derived &&> test(base && b)
{
    return std::tuple<derived &&>(std::move(b));
}

int main(int,char**)
{
    auto d = std::get<0>(test(derived{}));
    return 0;
}
