// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

struct bidi_iterator
{
  // No nested reference and pointer types.
  // No iterator_category.

  // cpp17-iterator requirements:
  int&           operator*() const;
  bidi_iterator& operator++();
  bidi_iterator  operator++(int);

  // cpp17-input-iterator requirements:
  friend bool operator==(const bidi_iterator&, const bidi_iterator&);
  using difference_type = long long;
  using value_type = int;

  // cpp17-forward-iterator requirements:
  bidi_iterator();

  // cpp17-bidirectional-iterator requirements:
  bidi_iterator& operator--();
  bidi_iterator operator--(int);
};

void
test01()
{
  // PR libstdc++/97935
  // Missing subsumption in iterator category detection
  using namespace std;
  static_assert(__detail::__cpp17_bidi_iterator<bidi_iterator>);
  static_assert(same_as<iterator_traits<bidi_iterator>::iterator_category,
			bidirectional_iterator_tag>,
		"PR libstdc++/97935");
}
