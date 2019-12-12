// 2007-04-27  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

// { dg-do compile }
// { dg-prune-output "cannot convert" }
// { dg-prune-output "no matching function .*_M_fill_initialize" }

#include <list>

void f()
{
  typedef std::list<std::list<int> > list_type;
  list_type l(10, 1);		// { dg-error "here|no match" }
}

// { dg-prune-output "iterator_traits" }
