// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2008 Free Software Foundation, Inc.
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

#include <cstdatomic>

namespace gnu
{
  using std::atomic_flag_test_and_set;
  using std::atomic_flag_test_and_set_explicit;
  using std::atomic_flag_clear;
  using std::atomic_flag_clear_explicit;
  using std::atomic_flag_fence;

  using std::atomic_global_fence_compatibility;

  // Sloppy testing for integral types (en masse).
  using std::atomic_is_lock_free;
  using std::atomic_store;
  using std::atomic_store_explicit;
  using std::atomic_load;
  using std::atomic_load_explicit;
  using std::atomic_swap;
  using std::atomic_swap_explicit;
  using std::atomic_compare_swap;
  using std::atomic_compare_swap_explicit;
  using std::atomic_fence;

  using std::atomic_fetch_add;
  using std::atomic_fetch_add_explicit;
  using std::atomic_fetch_sub;
  using std::atomic_fetch_sub_explicit;
}
