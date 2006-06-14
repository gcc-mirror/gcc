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
 * @file dbg_ex_allocator_base.cpp
 * Contains a debugging, exception throwing, allocator used for tests.
 */

#include <util/regression/res_mng/dbg_ex_allocator_base.hpp>

namespace pb_ds
{

  namespace test
  {

    namespace detail
    {

      twister_rand_gen dbg_ex_allocator_base::s_g;

      dbg_ex_allocator_base::map_t
      dbg_ex_allocator_base::s_map;

      double dbg_ex_allocator_base::s_throw_prob;

      size_t dbg_ex_allocator_base::s_label = 0;

      dbg_ex_allocator_base::entry_t
      dbg_ex_allocator_base::
      make_entry(void* p_r, size_t size)
      {
	return std::make_pair(
			      p_r,
			      alloc_data_t(s_label, size));
      }

      void
      dbg_ex_allocator_base::
      init(unsigned long seed)
      {
	s_g.init(seed);
      }

      void
      dbg_ex_allocator_base::
      set_throw_prob(double throw_prob)
      {
	s_throw_prob = throw_prob;
      }

      double
      dbg_ex_allocator_base::
      get_throw_prob()
      {
	return (s_throw_prob);
      }

      void
      dbg_ex_allocator_base::
      set_label(size_t l)
      {
	s_label = l;
      }

      void
      dbg_ex_allocator_base::
      insert(void* p_r, size_t size)
      {
	const_iterator found_it = s_map.find(p_r);

	if (found_it != s_map.end())
	  {
	    std::cerr << "Double insert! " << std::endl;
	    print_to_ostream(std::cerr, make_entry(p_r, size));
	    print_to_ostream(std::cerr, * found_it);

	    throw std::logic_error("double insert");
	  }

	s_map.insert(make_entry(p_r, size));
      }

      bool
      dbg_ex_allocator_base::
      empty()
      {
	return (s_map.empty());
      }

      void
      dbg_ex_allocator_base::
      erase(void* p_r, size_t size)
      {
	check_allocated(p_r, size);

	s_map.erase(p_r);
      }

      void
      dbg_ex_allocator_base::
      check_allocated(void* p_r, size_t size)
      {
	const_iterator found_it =
	  s_map.find(p_r);

	if (found_it == s_map.end())
	  {
	    std::cerr << "Null erase! " << std::endl;
	    print_to_ostream(std::cerr, make_entry(p_r, size));

	    throw std::logic_error("null erase");
	  }

	if (found_it->second.second != size)
	  {
	    std::cerr << "Wrong-size erase! " << std::endl;
	    print_to_ostream(std::cerr, make_entry(p_r, size));
	    print_to_ostream(std::cerr, * found_it);

	    throw std::logic_error("wrong-size erase");
	  }
      }

      void
      dbg_ex_allocator_base::
      cond_throw()
      {
	if (s_g.get_prob() < s_throw_prob)
	  throw forced_exception();
      }

      void
      dbg_ex_allocator_base::
      print_to_ostream(std::ostream& r_os)
      {
	const_iterator it =
	  dbg_ex_allocator_base::s_map.begin();

	const_iterator end_it =
	  dbg_ex_allocator_base::s_map.end();

	for (; it != end_it; ++it)
	  print_to_ostream(r_os, * it);

	r_os << std::endl;
      }

      void
      dbg_ex_allocator_base::
      print_to_ostream(std::ostream& r_os, const_reference r_entry)
      {
	r_os << r_entry.first << ": ";
	r_os << r_entry.second.first << ", ";
	r_os << r_entry.second.second;

	r_os << std::endl;
      }

      std::ostream& 
      operator<<(std::ostream& r_os, const dbg_ex_allocator_base& /*r_dbg*/)
      {
	dbg_ex_allocator_base::print_to_ostream(r_os);

	return r_os;
      }

    } // namespace detail

  } // namespace test

} // namespace pb_ds
