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
 * @file basic_multimap_example.cpp
 * A basic example showing how to use multimaps.
 */

// For cout, endl
#include <iostream>
// For various associative containers.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For compound_data_type and null_data_type.
#include <ext/pb_assoc/data_type.hpp>
// For assert
#include <cassert>

template<class Cntnr>
void
some_op_sequence(Cntnr c)
{
  assert(c.empty());
  assert(c.size() == 0);

  c[1].insert('a');

  c[2].insert('b');
  c[2].insert('c');

  assert(!c.empty());
  assert(c.size() == 2);

  std::cout << "Key 1 is mapped to " <<
    static_cast<unsigned long>(c[1].size()) << " entries." << std::endl;

  std::cout << "Key 2 is mapped to " <<
    static_cast<unsigned long>(c[2].size()) << " entries." << std::endl;

  std::cout << std::endl << "All values in the container:" << std::endl;

  for (typename Cntnr::const_iterator it = c.begin(); it != c.end();
       ++it)
    {
      std::cout << it->first << "->{ ";

      for (typename Cntnr::data_type::const_iterator it_ = it->second.begin();
	   it_  != it->second.end(); ++it_)
	std::cout <<* it_ << " ";

      std::cout << "}" << std::endl;
    }

  std::cout << std::endl;

  c.clear();

  assert(c.empty());
  assert(c.size() == 0);
}

int
main()
{
  /*
   * Perform operations on a collision-chaining hash-table of of
   *	red-black trees.
   */
  some_op_sequence(pb_assoc::cc_hash_assoc_cntnr<
		   int,
		   pb_assoc::compound_data_type<
		   pb_assoc::tree_assoc_cntnr<
		   char,
		   pb_assoc::null_data_type> > >());
}
