// { dg-do compile }

// 2005-02-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

// 6.3.4.6 unordered_multimap

#include <string>
#include <tr1/unordered_map>

using namespace std::tr1;
using std::string;
using std::equal_to;
using std::allocator;
using std::pair;

template class std::tr1::unordered_multimap<string, float>;
template class std::tr1::unordered_multimap<string, int,
				  hash<string>, equal_to<string>,
				  allocator<pair<const string, int> > >;
template class std::tr1::unordered_multimap<string, float,
				  hash<string>, equal_to<string>,
				  allocator<char> >;
template class std::tr1::__unordered_multimap<string, int,
				    hash<string>, equal_to<string>,
				    allocator<pair<const string, int> >, true>;
