// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2022 Free Software Foundation, Inc.
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

// C++11 20.8.3.4 reference_wrapper invocation [refwrap.invoke]
#include <functional>

struct ABC
{
    virtual bool operator()() const = 0;
};

struct Concrete : ABC
{
    virtual bool operator()() const { return true; }
};

Concrete c;
ABC& abc = c;

// PR libstdc++/57336 Cannot INVOKE a reference_wrapper around an abstract type
auto b = std::cref(abc)();
