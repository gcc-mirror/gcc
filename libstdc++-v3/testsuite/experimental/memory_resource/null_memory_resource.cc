// { dg-do run { target c++14 } }

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

#include <experimental/memory_resource>
#include <testsuite_hooks.h>

using std::experimental::pmr::memory_resource;
using std::experimental::pmr::null_memory_resource;
using std::experimental::pmr::new_delete_resource;

// null_memory_resource
void
test06()
{
  memory_resource* r = null_memory_resource();
  bool caught = false;

  void* p __attribute__((unused)) = nullptr;
  try {
    p = r->allocate(1);
  } catch (const std::bad_alloc&) {
    caught = true;
  }
  VERIFY( caught );

  VERIFY( *r == *r );
  VERIFY( r->is_equal(*r) );
  VERIFY( !r->is_equal(*new_delete_resource()) );
}

int main()
{
  test06();
}
