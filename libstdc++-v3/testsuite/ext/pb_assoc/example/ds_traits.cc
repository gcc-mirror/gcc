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
 * @file ds_traits_example.cpp
 * A basic example showing how to use ds_traits for querying container types
 *	for their behavior.
 */

// For various associative containers.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For ds_traits.
#include <ext/pb_assoc/ds_trait.hpp>
// For cout, endl.
#include <iostream>

template<class DS_Category>
void
print_ds_category(DS_Category);

template<>
void
print_ds_category(pb_assoc::cc_hash_ds_tag)
{
  std::cout << "Collision-chaining hash based associative-container:" <<
    std::endl;
}

template<>
void
print_ds_category(pb_assoc::gp_hash_ds_tag)
{
  std::cout << "Probing hash based associative-container:" <<
    std::endl;
}

template<>
void
print_ds_category(pb_assoc::rb_tree_ds_tag)
{
  std::cout << "Red-black tree associative-container:" <<
    std::endl;
}

template<>
void
print_ds_category(pb_assoc::splay_tree_ds_tag)
{
  std::cout << "Splay tree associative-container:" <<
    std::endl;
}

template<>
void
print_ds_category(pb_assoc::ov_tree_ds_tag)
{
  std::cout << "Ordered-vector tree associative-container:" <<
    std::endl;
}

template<>
void
print_ds_category(pb_assoc::lu_ds_tag)
{
  std::cout << "List-based associative-container:" <<
    std::endl;
}

void
print_erase_can_throw(bool can)
{
  if (can)
    {
      std::cout << "Erase can throw" << std::endl;

      return;
    }

  std::cout << "Erase cannot throw" << std::endl;
}

void
print_order_preserving(bool does)
{
  if (does)
    {
      std::cout << "Preserves order" << std::endl;

      return;
    }

  std::cout << "Does not preserve order" << std::endl;
}

void
print_erase_iterators(bool can)
{
  if (can)
    {
      std::cout << "Can erase iterators" << std::endl;

      return;
    }

  std::cout << "Cannot erase iterators" << std::endl;
}

template<class Invalidation_Guarantee>
void
print_invalidation_guarantee(Invalidation_Guarantee);

template<>
void
print_invalidation_guarantee(pb_assoc::basic_invalidation_guarantee)
{
  std::cout << "Guarantees only that found references, pointers, and "
    "iterators are valid as long as the container object is not "
    "modified" << std::endl;
}

template<>
void
print_invalidation_guarantee(pb_assoc::find_invalidation_guarantee)
{
  std::cout << "Guarantees that found references, pointers, and "
    "find_iterators are valid even if the container object "
    "is modified" << std::endl;
}

template<>
void
print_invalidation_guarantee(pb_assoc::range_invalidation_guarantee)
{
  std::cout << "Guarantees that iterators remain valid even if the "
    "container object is modified" << std::endl;
}

void
print_reverse_iteration(bool does)
{
  if (does)
    {
      std::cout << "Supports reverse iteration" << std::endl;

      return;
    }

  std::cout << "Does not support reverse iteration" << std::endl;
}

void
print_split_join(bool does)
{
  if (does)
    {
      std::cout << "Supports split and join" << std::endl;

      return;
    }

  std::cout << "Does not support split and join" << std::endl;
}

template<class Cntnr>
void
print_container_attributes()
{
  // First print out the data-structure category.

  print_ds_category(typename Cntnr::ds_category());

  // Next is the data-structure traits class of the container.

  typedef pb_assoc::ds_traits< Cntnr> traits;

  // Now print the attributes of the container.

  print_erase_can_throw(traits::erase_can_throw);

  print_order_preserving(traits::order_preserving);

  print_erase_iterators(traits::erase_iterators);

  print_invalidation_guarantee(typename traits::invalidation_guarantee());

  print_reverse_iteration(traits::reverse_iteration);

  print_split_join(traits::split_join);

  std::cout << std::endl << std::endl;
}

int
main()
{
  print_container_attributes<pb_assoc::cc_hash_assoc_cntnr<int, char> >();

  print_container_attributes<pb_assoc::gp_hash_assoc_cntnr<int, char> >();

  print_container_attributes<pb_assoc::tree_assoc_cntnr<int, char> >();

  print_container_attributes<pb_assoc::tree_assoc_cntnr<
    int,
    char,
    std::less<int>,
    pb_assoc::splay_tree_ds_tag> >();

  print_container_attributes<pb_assoc::tree_assoc_cntnr<
    int,
    char,
    std::less<int>,
    pb_assoc::ov_tree_ds_tag> >();

  print_container_attributes<pb_assoc::lu_assoc_cntnr<int, char> >();

  typedef
    pb_assoc::lu_assoc_cntnr<
    int,
    pb_assoc::compound_data_type<
    pb_assoc::gp_hash_assoc_cntnr<
    char,
    pb_assoc::null_data_type> > >
    mmap_t;

  print_container_attributes<mmap_t>();
}
