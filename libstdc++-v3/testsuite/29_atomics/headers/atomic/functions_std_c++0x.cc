// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2008-2013 Free Software Foundation, Inc.
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

#include <atomic>

namespace gnu
{
  using std::atomic_flag_test_and_set;
  using std::atomic_flag_test_and_set_explicit;
  using std::atomic_flag_clear;
  using std::atomic_flag_clear_explicit;

  using std::kill_dependency;

  // Sloppy testing for integral types (en masse).
  using std::atomic_is_lock_free;
  using std::atomic_store;
  using std::atomic_store_explicit;
  using std::atomic_load;
  using std::atomic_load_explicit;
  using std::atomic_exchange;
  using std::atomic_exchange_explicit;
  using std::atomic_compare_exchange_weak;
  using std::atomic_compare_exchange_strong;
  using std::atomic_compare_exchange_weak_explicit;
  using std::atomic_compare_exchange_strong_explicit;

  using std::atomic_fetch_add;
  using std::atomic_fetch_add_explicit;
  using std::atomic_fetch_sub;
  using std::atomic_fetch_sub_explicit;
}
