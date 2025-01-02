// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

#include <tr1/memory>
#include <functional>
#include <tr1/functional>

std::tr1::shared_ptr<int> test() { return {}; }

std::function<std::tr1::shared_ptr<int>()> func = test;
std::function<std::tr1::shared_ptr<int>()> funcr = std::ref(test);

void test2(std::tr1::shared_ptr<int>) { }

std::function<void(std::tr1::shared_ptr<int>)> func2 = std::ref(test2);
