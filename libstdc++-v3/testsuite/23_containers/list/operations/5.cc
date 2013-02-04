// 2006-01-19  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2013 Free Software Foundation, Inc.
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

#include "5.h"
#include <list>

int main()
{
  typedef int value_type;
  typedef __gnu_test::uneq_allocator<value_type> allocator_type;
  typedef std::list<value_type, allocator_type> list_type;

  operations05<list_type>();
  return 0;
}
