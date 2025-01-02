// { dg-do compile { target c++14 } }

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

#include <experimental/memory>

using std::experimental::observer_ptr;

struct nontrivial {nontrivial() {}};
struct other {};
struct base {};
struct derived : base {};

static_assert(!std::is_trivially_constructible<
              observer_ptr<nontrivial>>::value, "");
static_assert(std::is_trivially_copyable<
              observer_ptr<nontrivial>>::value, "");
static_assert(std::is_trivially_destructible<
              observer_ptr<nontrivial>>::value, "");

static_assert(std::is_constructible<
              observer_ptr<nontrivial>, nontrivial*>::value,
              "");
static_assert(std::is_constructible<observer_ptr<base>, base*>::value, "");
static_assert(std::is_constructible<observer_ptr<base>, derived*>::value, "");
static_assert(!std::is_constructible<observer_ptr<base>, other*>::value, "");
static_assert(std::is_constructible<
                observer_ptr<base>, observer_ptr<base>>::value, "");
static_assert(std::is_constructible<
                observer_ptr<base>, observer_ptr<derived>>::value, "");
static_assert(!std::is_constructible<
                observer_ptr<base>, observer_ptr<other>>::value, "");

static_assert(!std::is_assignable<
              observer_ptr<nontrivial>, nontrivial*>::value,
              "");
static_assert(std::is_assignable<
              observer_ptr<nontrivial>, observer_ptr<nontrivial>>::value,
              "");
static_assert(std::is_assignable<observer_ptr<base>, 
              observer_ptr<base>>::value, "");
static_assert(std::is_assignable<observer_ptr<base>, 
              observer_ptr<derived>>::value, "");
static_assert(!std::is_assignable<
                observer_ptr<base>, observer_ptr<other>>::value, "");
static_assert(std::is_assignable<observer_ptr<const int>, 
              observer_ptr<int>>::value, "");
static_assert(!std::is_assignable<observer_ptr<int>, 
              observer_ptr<const int>>::value, "");
