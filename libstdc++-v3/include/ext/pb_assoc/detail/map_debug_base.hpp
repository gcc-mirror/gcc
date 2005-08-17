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
 * @file map_debug_base.hpp
 * Contains a debug-mode base for all maps.
 */

#ifndef MAP_DEBUG_BASE_HPP
#define MAP_DEBUG_BASE_HPP

#ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

#include <assert.h>
#include <utility>
#include <set>
#include <pb_assoc/testsuite/regression/res_mng/dbg_ex_allocator_base.hpp>

namespace pb_assoc
{

  namespace detail
  {

#ifdef PB_ASSOC_MAP_DEBUG_BASE_DEBUG
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_MAP_DEBUG_BASE_DEBUG
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_MAP_DEBUG_BASE_DEBUG

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Key, class Eq_Fn>

#define PB_ASSOC_CLASS_C_DEC \
	map_debug_base< \
		Key, \
		Eq_Fn>

    template<typename Key, class Eq_Fn>
    class map_debug_base
    {
    private:
      typedef typename std::allocator<Key> key_allocator;

      typedef typename key_allocator::size_type size_type;

      typedef typename key_allocator::const_reference const_key_reference;

    protected:
      map_debug_base();

      map_debug_base(const PB_ASSOC_CLASS_C_DEC& r_other);

      ~map_debug_base();

      inline void
      insert_new(const_key_reference r_key);

      inline void
      insert_existing(const_key_reference r_key);

      inline void
      erase_existing(const_key_reference r_key);

      void
      clear();

      inline void
      check_key_exists(const_key_reference r_key) const;

      inline void
      check_key_does_not_exist(const_key_reference r_key) const;

      inline void
      check_size(size_type size) const;

      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

    private:
      typedef std::set<Key> key_set;

    private:
      key_set m_key_set;
    };

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    map_debug_base()
    {

    }

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    map_debug_base(const PB_ASSOC_CLASS_C_DEC& r_other) :
      m_key_set(r_other.m_key_set)
    { }

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    ~map_debug_base()
    { }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    insert_new(const_key_reference r_key)
    {
      const double orig_throw_prob =
	pb_assoc::detail::test::dbg_ex_allocator_base().get_throw_prob();

      pb_assoc::detail::test::dbg_ex_allocator_base().
	set_throw_prob(0);

      if (m_key_set.find(r_key) != m_key_set.end())
	abort();

      try
	{
	  m_key_set.insert(r_key);
	}
      catch(...)
	{
	  pb_assoc::detail::test::dbg_ex_allocator_base().
	    set_throw_prob(orig_throw_prob);

	  throw;
	}

      pb_assoc::detail::test::dbg_ex_allocator_base().
	set_throw_prob(orig_throw_prob);
    }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    erase_existing(const_key_reference r_key)
    {
      if (m_key_set.find(r_key) == m_key_set.end())
	abort();

      m_key_set.erase(r_key);

      if (m_key_set.find(r_key) != m_key_set.end())
	abort();
    }

    PB_ASSOC_CLASS_T_DEC
    void
    PB_ASSOC_CLASS_C_DEC::
    clear()
    {
      m_key_set.clear();
    }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    check_key_exists(const_key_reference r_key) const
    {
      if (m_key_set.find(r_key) == m_key_set.end())
	abort();
    }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    check_key_does_not_exist(const_key_reference r_key) const
    {
      if (m_key_set.find(r_key) != m_key_set.end())
	abort();
    }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    check_size(size_type size) const
    {
      const size_type key_set_size = m_key_set.size();

      if (size != key_set_size)
	abort();
    }

    PB_ASSOC_CLASS_T_DEC
    void
    PB_ASSOC_CLASS_C_DEC::
    swap(PB_ASSOC_CLASS_C_DEC& r_other)
    {
      m_key_set.swap(r_other.m_key_set);
    }

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

  } // namespace detail

} // namespace pb_assoc

#endif // #ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

#endif // #ifndef MAP_DEBUG_BASE_HPP

