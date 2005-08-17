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
 * @file basic_set_example.cpp
 * A basic example showing how to use sets.
 */

// For various associative containers.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For null_data_type
#include <ext/pb_assoc/data_type.hpp>
// For cout, endl
#include <iostream>
// For assert
#include <cassert>

template<class Cntnr>
void
some_op_sequence(Cntnr c)
{
  assert(c.empty());
  assert(c.size() == 0);

  c.insert(1);
  c.insert(2);

  assert(!c.empty());
  assert(c.size() == 2);

  std::cout << "All value types in the container:" << std::endl;
  for (typename Cntnr::const_iterator it = c.begin(); it != c.end();
       ++it)
    std::cout <<* it << " ";

  std::cout << std::endl;

  c.clear();

  assert(c.empty());
  assert(c.size() == 0);
}

int
main()
{
  /*
   * Perform operations on a collision-chaining hash set.
   */
  some_op_sequence(pb_assoc::cc_hash_assoc_cntnr<int, pb_assoc::null_data_type>());

  /*
   * Perform operations on a general-probing hash set.
   */
  some_op_sequence(pb_assoc::gp_hash_assoc_cntnr<int, pb_assoc::null_data_type>());

  /*
   * Perform operations on a red-black tree set.
   */
  some_op_sequence(pb_assoc::tree_assoc_cntnr<int, pb_assoc::null_data_type>());

  /*
   * Perform operations on a splay tree set.
   */
  some_op_sequence(pb_assoc::tree_assoc_cntnr<
		   int,
		   pb_assoc::null_data_type,
		   std::less<int>,
		   pb_assoc::splay_tree_ds_tag>());

  /*
   * Perform operations on a splay tree set.
   */
  some_op_sequence(pb_assoc::tree_assoc_cntnr<
		   int,
		   pb_assoc::null_data_type,
		   std::less<int>,
		   pb_assoc::ov_tree_ds_tag>());

  /*
   * Perform operations on a list-update set.
   */
  some_op_sequence(pb_assoc::lu_assoc_cntnr<int, pb_assoc::null_data_type>());
}
