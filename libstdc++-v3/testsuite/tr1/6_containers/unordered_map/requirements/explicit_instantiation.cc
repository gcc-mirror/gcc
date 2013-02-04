// { dg-do compile }

// 2005-02-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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

// 6.3.4.4 unordered_map

#include <string>
#include <tr1/unordered_map>

using namespace std::tr1;
using std::string;
using std::allocator;
using std::pair;
using std::equal_to;

template class unordered_map<string, float>;
template class unordered_map<string, int,
			     hash<string>, equal_to<string>, 
			     allocator<pair<const string, int> > >;
template class unordered_map<string, float,
			     hash<string>, equal_to<string>, 
			     allocator<char> >;
template class __unordered_map<string, int,
			       hash<string>, equal_to<string>, 
			       allocator<pair<const string, int> >, true>;
