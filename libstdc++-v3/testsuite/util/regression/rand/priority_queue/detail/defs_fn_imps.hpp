// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file defs_fn_imps.hpp
 * Containsert a random regression test for a specific container type.
 */

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
defs()
{
  // General container types.

  typedef typename Cntnr::size_type test_size_type;

  typedef typename Cntnr::difference_type difference_type;

  value_defs();

  iterator_defs();

  policy_defs();
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
value_defs()
{
  typedef typename Cntnr::value_type test_value_type;

  typedef typename Cntnr::reference test_reference;

  typedef typename Cntnr::const_reference test_const_reference;

  typedef typename Cntnr::pointer test_pointer;

  typedef typename Cntnr::const_pointer test_const_pointer;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
ds_defs()
{
  typedef typename Cntnr::container_category test_container_category;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
iterator_defs()
{
  typedef typename Cntnr::point_iterator test_point_iterator;

  typedef typename Cntnr::const_point_iterator const_test_point_iterator;

  typedef typename Cntnr::iterator test_iterator;

  typedef typename Cntnr::const_iterator const_test_iterator;
}

PB_DS_CLASS_T_DEC
void
PB_DS_CLASS_C_DEC::
policy_defs()
{
  typedef typename Cntnr::allocator test_allocator;

  typedef typename Cntnr::cmp_fn test_cmp_fn;
}
