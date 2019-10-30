// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <iterator>

using std::output_iterator;

static_assert( output_iterator< int*, int > );
static_assert( output_iterator< int*, const int > );
static_assert( output_iterator< int*, long > );
static_assert( output_iterator< void**, void* > );
static_assert( output_iterator< void**, long* > );
static_assert( ! output_iterator< const int*, int > );

static_assert( ! output_iterator< int* const, int > );
static_assert( ! output_iterator< const int* const, int > );
static_assert( ! output_iterator< void** const, void* > );

static_assert( ! output_iterator< void*, void > );
static_assert( ! output_iterator< const void*, void > );
static_assert( ! output_iterator< const void*, void* > );
static_assert( ! output_iterator< volatile void*, void > );
static_assert( ! output_iterator< volatile void*, void* > );

static_assert( ! output_iterator< void(*)(), void(&)() > );
static_assert( ! output_iterator< void(&)(), void(&)() > );
static_assert( output_iterator< void(**)(), void(*)() > );
static_assert( output_iterator< void(**)(), void(&)() > );

struct A;
static_assert( ! output_iterator< void(A::*)(), A* > );
static_assert( ! output_iterator< void(A::*)(), void(A::*)() > );
static_assert( ! output_iterator< int A::*, int > );
static_assert( ! output_iterator< int A::*, int A::* > );

#include <array>
#include <deque>
#include <forward_list>
#include <list>
#include <set>
#include <string>
#include <string_view>
#include <vector>

using std::array;
using std::deque;
using std::forward_list;
using std::list;
using std::set;
using std::string;
using std::string_view;
using std::vector;

struct B { };

static_assert( output_iterator< array<int, 1>::iterator, int > );
static_assert( output_iterator< array<B, 1>::iterator, B > );
static_assert( ! output_iterator< array<int, 1>::const_iterator, int > );
static_assert( ! output_iterator< array<B, 1>::const_iterator, B > );

static_assert( output_iterator< deque<int>::iterator, int > );
static_assert( output_iterator< deque<B>::iterator, B > );
static_assert( ! output_iterator< deque<int>::const_iterator, int > );
static_assert( ! output_iterator< deque<B>::const_iterator, B > );

static_assert( output_iterator< forward_list<int>::iterator, int > );
static_assert( output_iterator< forward_list<B>::iterator, B > );
static_assert( ! output_iterator< forward_list<int>::const_iterator, int > );
static_assert( ! output_iterator< forward_list<B>::const_iterator, B > );

static_assert( output_iterator< list<int>::iterator, int > );
static_assert( output_iterator< list<B>::iterator, B > );
static_assert( ! output_iterator< list<int>::const_iterator, int > );
static_assert( ! output_iterator< list<B>::const_iterator, B > );

static_assert( ! output_iterator< set<int>::iterator, int > );
static_assert( ! output_iterator< set<B>::iterator, B > );
static_assert( ! output_iterator< set<int>::const_iterator, int > );
static_assert( ! output_iterator< set<B>::const_iterator, B > );

static_assert( output_iterator< string::iterator, char > );
static_assert( output_iterator< string::iterator, int > );
static_assert( ! output_iterator< string::const_iterator, char > );
static_assert( ! output_iterator< string::const_iterator, int > );

static_assert( ! output_iterator< string_view::iterator, char > );
static_assert( ! output_iterator< string_view::iterator, int > );
static_assert( ! output_iterator< string_view::const_iterator, char > );
static_assert( ! output_iterator< string_view::const_iterator, int > );

static_assert( output_iterator< vector<int>::iterator, int > );
static_assert( output_iterator< vector<B>::iterator, B > );
static_assert( ! output_iterator< vector<int>::const_iterator, int > );
static_assert( ! output_iterator< vector<B>::const_iterator, B > );

#include <streambuf>

using std::istreambuf_iterator;
using std::ostreambuf_iterator;

static_assert( ! output_iterator< istreambuf_iterator<char>, char > );
static_assert( ! output_iterator< istreambuf_iterator<char>, int > );
static_assert( output_iterator< ostreambuf_iterator<char>, char > );
static_assert( output_iterator< ostreambuf_iterator<char>, int > );
