// -*- C++ -*-

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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice and
// this permission notice appear in supporting documentation. None of
// the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied warranty.

/**
 * @file erase_if_example.cpp
 * A basic example showing how to use erase_if.
 */

// For various associative containers.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For cout, endl.
#include <iostream>
// For assert.
#include <cassert>

struct between : public std::unary_function<
  std::pair<const int, char>,
		 bool>
{
  between(int b, int e) : m_b(b),
			  m_e(e)
  { }

  inline bool
  operator()(std::pair<const int, char>& r_val)
  {
    return (r_val.first >= m_b&&  r_val.first < m_e);
  }

private:
  const int m_b;
  const int m_e;
};

template<class Cntnr>
void
some_op_sequence(Cntnr c)
{
  assert(c.empty());
  assert(c.size() == 0);

  for (int i = 0; i < 100; ++i)
    c.insert(std::make_pair(i, static_cast<char>(i)));

  assert(c.size() == 100);

  /* Erase all values whose key is between 10 (inclusive)
   * and 90 (non-inclusive).
   */
  c.erase_if(between(10 , 90));

  assert(!c.empty());
  assert(c.size() == 20);
}

int
main()
{

  /*
   * Perform operations on a list-update set.
   */
  some_op_sequence(pb_assoc::lu_assoc_cntnr<int, char>());

  /*
   * Perform operations on a collision-chaining hash set.
   */
  some_op_sequence(pb_assoc::cc_hash_assoc_cntnr<int, char>());

  /*
   * Perform operations on a general-probing hash set.
   */
  some_op_sequence(pb_assoc::gp_hash_assoc_cntnr<int, char>());

  /*
   * Perform operations on a red-black tree set.
   */
  some_op_sequence(pb_assoc::tree_assoc_cntnr<int, char>());

  /*
   * Perform operations on a splay tree set.
   */
  some_op_sequence(pb_assoc::tree_assoc_cntnr<
		   int,
		   char,
		   std::less<int>,
		   pb_assoc::splay_tree_ds_tag>());

  /*
   * Perform operations on a splay tree set.
   */
  some_op_sequence(pb_assoc::tree_assoc_cntnr<
		   int,
		   char,
		   std::less<int>,
		   pb_assoc::ov_tree_ds_tag>());
}
