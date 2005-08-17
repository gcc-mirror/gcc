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
 * @file tree_join_example.cpp.cpp
 * An example showing how to join splay_tree_map objects.
 *	The code in the example is relevant to red-black trees as well.
 */

// For tree_assoc_cntnr
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For cannot_join exception.
#include <ext/pb_assoc/exception.hpp>
// For assert
#include <cassert>

int
main()
{
  /*
   * A splay tree table mapping ints to chars.
   */
  typedef
    pb_assoc::tree_assoc_cntnr<
    int,
    char,
    std::less<
    int>,
    pb_assoc::splay_tree_ds_tag>
    map_t;

  // Two map_t object.
  map_t h0, h1;

  // Insert some values into the first table.
  for (int i0 = 0; i0 < 100; ++i0)
    h0.insert(std::make_pair(i0, 'a'));

  // Insert some values into the second table.
  for (int i1 = 0; i1 < 100; ++i1)
    h1.insert(std::make_pair(1000 + i1, 'b'));

  /*
   * Since all the elements in h0 are smaller than those in h1,
   *	it is possible to join the two tables. This is exception free.
   */

  h0.join(h1);

  // h0 should now contain all entries, and h1 should be empty.

  assert(h0.size() == 200);
  assert(h1.empty());

  // Now perform an illegal join.

  // Two other map_t objects.
  map_t h2, h3;

  h2[1] = 'a';
  h2[3] = 'c';

  h3[2] = 'b';

  /*
   * It is not true that all elements in h2 are smaller than those
   *	in h3, nor is it true that they are all larger. Hence, attempting
   *	to join h2, and h3 should result in an exception.
   */

  bool exception_thrown = false;

  try
    {
      h2.join(h3);
    }
  catch(pb_assoc::cannot_join& )
    {
      exception_thrown = true;
    }

  assert(exception_thrown);

  // Since the operation was not performed, the tables should be unaltered.

  assert(h2.size() == 2);
  assert(h3[2] == 'b');

  return (0);
}

