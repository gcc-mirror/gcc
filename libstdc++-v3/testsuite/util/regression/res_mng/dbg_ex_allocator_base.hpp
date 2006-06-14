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
 * @file dbg_ex_allocator_base.hpp
 * Contains a debugging, exception throwing, allocator used for tests.
 */

#ifndef PB_DS_DBG_EX_ALLOCATOR_BASE_HPP
#define PB_DS_DBG_EX_ALLOCATOR_BASE_HPP

#include <math.h>
#include <map>
#include <set>
#include <iostream>
#include <stdexcept>
#include <regression/res_mng/forced_exception.hpp>
#include <rng/twister_rand_gen.hpp>

namespace pb_ds
{

  namespace test
  {

    namespace detail
    {

      class dbg_ex_allocator_base;

      std::ostream& 
      operator<<(std::ostream& r_os, const dbg_ex_allocator_base& r_dbg);

      class dbg_ex_allocator_base
      {
      public:
	typedef size_t label;

      public:
	void
        init(unsigned long seed);

	static void
        set_throw_prob(double throw_prob);

	static double
        get_throw_prob();

	static void
        set_label(size_t l);

	static bool
        empty();

	class group_throw_prob_adjustor
	{
	public:
	  group_throw_prob_adjustor(size_t size) : m_orig_throw_prob(s_throw_prob)
	  {
            s_throw_prob =
	      1 - ::pow((double)(
				 1 - s_throw_prob),(double)(0.5 / (size + 1)));
	  }

	  ~group_throw_prob_adjustor()
	  {
            s_throw_prob = m_orig_throw_prob;
	  }

	private:
	  const double m_orig_throw_prob;
	};

	class zero_throw_prob_adjustor
	{
	public:
	  zero_throw_prob_adjustor() : m_orig_throw_prob(s_throw_prob)
	  {
            s_throw_prob = 0;
	  }

	  ~zero_throw_prob_adjustor()
	  {
            s_throw_prob = m_orig_throw_prob;
	  }

	private:
	  const double m_orig_throw_prob;
	};

      protected:
	static void
        insert(void* p_r, size_t size);

	static void
        erase(void* p_r, size_t size);

	static void
        cond_throw();

	static void
        assert_allocatod(const void* p_r, size_t size);

	static void
        check_allocated(void* p_r, size_t size);

      private:
	typedef std::pair< label, size_t> alloc_data_t;

	typedef std::map< void* , alloc_data_t> map_t;

	typedef map_t::value_type entry_t;

	typedef map_t::const_iterator const_iterator;

	typedef map_t::const_reference const_reference;

      private:
	static void
        print_to_ostream(std::ostream& r_os);

	static void
        print_to_ostream(std::ostream& r_os, const_reference r_entry);

	static entry_t
        make_entry(void* p_r, size_t size);

      private:
	static twister_rand_gen s_g;

	static map_t s_map;

	static double s_throw_prob;

	static size_t s_label;

	friend std::ostream& operator<<(std::ostream& r_os, const dbg_ex_allocator_base& r_dbg);
      };

      std::ostream& 
      operator<<(std::ostream& r_os, const dbg_ex_allocator_base& r_dbg);

    } // namespace detail

  } // namespace test

} // namespace pb_ds

#endif // #ifndef PB_DS_DBG_EX_ALLOCATOR_BASE_HPP
