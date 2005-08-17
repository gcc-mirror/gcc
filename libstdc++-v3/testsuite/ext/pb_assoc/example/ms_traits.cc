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
 * @file ms_traits_example.cpp
 * A basic example showing how to use ms_traits for querying container types
 *	for their behavior.
 */

// For various associative containers.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For ms_traits.
#include <ext/pb_assoc/ms_trait.hpp>
// For cout, endl.
#include <iostream>

void
print_has_data_compound_data(bool has_data, bool has_compound_data)
{
  if (!has_data)
    {
      std::cout << "Does not have data." << std::endl;

      return;
    }

  if (!has_compound_data)
    {
      std::cout << "Has data." << std::endl;

      return;
    }

  std::cout << "Has compound data." << std::endl;
}

void
print_mapping_level(unsigned int mapping_level)
{
  std::cout << "Mapping level = " << mapping_level << std::endl;
}

template<class Cntnr>
void
print_mapping_attributes()
{
  typedef pb_assoc::ms_traits< Cntnr> traits;

  print_has_data_compound_data(traits::has_data, traits::has_compound_data);

  print_mapping_level(traits::mapping_level);

  std::cout << std::endl << std::endl;
}

int
main()
{
  std::cout << "\"Set\"-type container:" << std::endl;

  print_mapping_attributes<
    pb_assoc::cc_hash_assoc_cntnr<int, pb_assoc::null_data_type> >();

  std::cout << "\"Map\"-type container:" << std::endl;

  print_mapping_attributes<
    pb_assoc::cc_hash_assoc_cntnr<int, char> >();

  typedef
    pb_assoc::cc_hash_assoc_cntnr<
    int,
    pb_assoc::compound_data_type<
    pb_assoc::tree_assoc_cntnr<
    char,
    pb_assoc::null_data_type> > >
    mmap_t;

  std::cout << "\"Multimap\"-type container:" << std::endl;

  print_mapping_attributes<mmap_t>();

  std::cout << "Rebound \"Multimap\"-type container:" << std::endl;

  print_mapping_attributes<mmap_t::rebind<1>::other>();
}
