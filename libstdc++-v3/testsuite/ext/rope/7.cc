// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// rope (SGI extension)

// { dg-do run }

#include <ext/rope>
#include <testsuite_hooks.h>

void
test01()
{
  using __gnu_cxx::crope;

  char str_a[] =
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  char str_b[] =
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";

  // Create ropes with leaf nodes longer than __lazy_threshold = 128
  // so substring nodes will be created by the next step
  crope leaf_rope_a(str_a);
  crope leaf_rope_b(str_b);

  // Create ropes with substring nodes referencing the leaf nodes
  // of the prior ropes
  crope substring_rope_a(leaf_rope_a.begin() + 1,
                         leaf_rope_a.begin() + 199);
  crope substring_rope_b(leaf_rope_b.begin() + 1,
                         leaf_rope_b.begin() + 199);

  // Create iterators to substring_rope_a
  crope::const_iterator cit_orig = substring_rope_a.begin();
  crope::iterator mit_orig = substring_rope_a.mutable_begin();

  // Dereference the iterators so they cache a portion of the substring
  // node in their internal buffers
  *cit_orig;
  *mit_orig;

  // Copy the original iterators, via both copy constructors and
  // assignment operators.  Prior to the bug fix, these iterators
  // retained pointers to the internal buffers of the original
  // iterators.
  crope::const_iterator cit_copy(cit_orig);
  crope::iterator mit_copy(mit_orig);
  crope::const_iterator cit_assign; cit_assign = cit_orig;
  crope::iterator mit_assign; mit_assign = mit_orig;

  // Modify the original iterators to refer to substring_rope_b
  cit_orig = substring_rope_b.begin();
  mit_orig = substring_rope_b.mutable_begin();

  // Dereference the original iterators so they fill their internal
  // buffers with part of substring_rope_b
  *cit_orig;
  *mit_orig;

  // Verify that the copied iterators return data from
  // substring_rope_a, not substring_rope_b
  VERIFY(*cit_copy == 'a');
  VERIFY(*mit_copy == 'a');
  VERIFY(*cit_assign == 'a');
  VERIFY(*mit_assign == 'a');
}

int
main()
{
  test01();
  return 0;
}
