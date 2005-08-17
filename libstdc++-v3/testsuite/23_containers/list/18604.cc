// 2005-05-09  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do compile }

// libstdc++/18604
struct less;
struct allocator;
struct pair;
struct binary_function;
struct iterator;
struct iterator_traits;
struct bidirectional_iterator_tag;
struct forward_iterator_tag;
struct input_iterator_tag;
struct random_access_iterator_tag;
struct ios_base;
struct basic_string;
struct basic_istream;
struct basic_ostream;
struct char_traits;

#include <list>
