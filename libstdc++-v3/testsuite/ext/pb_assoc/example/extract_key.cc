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
 * @file extract_key_example.cpp
 * An example of extracting keys from values.
 */

// For various associative containers
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For binary_function
#include <functional>
// For assert
#include <cassert>

using namespace std;

/*
 * A functor for checking whether two tables share exactly the
 *	same keys.
 */
template<class Cntnr0, class Cntnr1>
struct compare_keys : public std::binary_function<
  Cntnr0,
  Cntnr1,
  bool>
{
  bool
  operator()(const Cntnr0& r_c0, const Cntnr1& r_c1)
  {
    // If the tables' sizes differ, they cannot share the same keys.
    if (r_c0.size() != r_c1.size())
      return (false);

    // Loop and check each key.
    for (typename Cntnr0::const_iterator it = r_c0.begin();
	 it != r_c0.end(); ++it)
      if (r_c1.find(r_c0.extract_key(*it)) == r_c1.end())
	return (false);

    return (true);
  };
};

template<class Cntnr0, class Cntnr1>
bool
key_equiv(const Cntnr0& r_c0, const Cntnr1& r_c1)
{
  return (compare_keys<Cntnr0, Cntnr1>()(r_c0, r_c1));
}

int
main()
{
  /*
   * A collision-chaining hash-based associative container
   * 	mapping integers to characters.
   */
  pb_assoc::cc_hash_assoc_cntnr<int, char> c0;

  /*
   * A red-black tree-based associative container
   * 	mapping integers to doubles.
   */
  pb_assoc::tree_assoc_cntnr<int, double> c1;

  /*
   * A list-update associative container storing integers.
   */
  pb_assoc::lu_assoc_cntnr<int, pb_assoc::null_data_type> c2;

  /*
   * Since the objects are initially empty, they
   *	should all be equivalent.
   **/

  assert(key_equiv(c0, c0));
  assert(key_equiv(c0, c1));
  assert(key_equiv(c0, c2));

  // Insert some entries.

  c0.insert(std::make_pair(1, 'a'));

  c1.insert(std::make_pair(1, 'a'));
  c1.insert(std::make_pair(2, 'b'));

  c2.insert(1);

  // Only c0 and c2 contain a single key with value 1.

  assert(key_equiv(c0, c0));
  assert(key_equiv(c0, c1) == false);
  assert(key_equiv(c0, c2));
}

