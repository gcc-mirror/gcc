// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

using std::forward_iterator;

static_assert( forward_iterator< int*	    > );
static_assert( forward_iterator< const int* > );
static_assert( forward_iterator< void**	    > );

static_assert( ! forward_iterator< int* const	> );
static_assert( ! forward_iterator< const int* const	> );
static_assert( ! forward_iterator< void** const	> );

static_assert( ! forward_iterator< void*	  > );
static_assert( ! forward_iterator< const void*	  > );
static_assert( ! forward_iterator< volatile void* > );

static_assert( ! forward_iterator< void(*)() > );
static_assert( ! forward_iterator< void(&)() > );

struct A;
static_assert( ! forward_iterator< void(A::*)() > );
static_assert( ! forward_iterator< int A::*	> );

#include <array>
#include <deque>
#include <forward_list>
#include <list>
#include <string>
#include <string_view>
#include <vector>

using std::array;
using std::deque;
using std::forward_list;
using std::list;
using std::string;
using std::string_view;
using std::vector;
using std::istreambuf_iterator;
using std::ostreambuf_iterator;

struct B { };

static_assert( forward_iterator< array<int, 1>::iterator      > );
static_assert( forward_iterator< array<B, 1>::const_iterator  > );

static_assert( forward_iterator< deque<int>::iterator	  > );
static_assert( forward_iterator< deque<B>::const_iterator > );

static_assert( forward_iterator< forward_list<int>::iterator	  > );
static_assert( forward_iterator< forward_list<B>::const_iterator  > );

static_assert( forward_iterator< list<int>::iterator	  > );
static_assert( forward_iterator< list<B>::const_iterator  > );

static_assert( forward_iterator< string::iterator	> );
static_assert( forward_iterator< string::const_iterator	> );

static_assert( forward_iterator< string_view::iterator	      > );
static_assert( forward_iterator< string_view::const_iterator  > );

static_assert( forward_iterator< vector<int>::iterator	    > );
static_assert( forward_iterator< vector<B>::const_iterator  > );

static_assert( ! forward_iterator< istreambuf_iterator<char> > );
static_assert( ! forward_iterator< ostreambuf_iterator<char> > );
