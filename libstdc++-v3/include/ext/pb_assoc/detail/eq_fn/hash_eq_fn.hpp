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
 * @file hash_eq_fn.hpp
 * Contains 2 eqivalence functions, one employing a hash value,
 *	and one ignoring it.
 */

#ifndef HASH_EQ_FN_HPP
#define HASH_EQ_FN_HPP

#include <utility>

namespace pb_assoc
{

  namespace detail
  {

#ifdef PB_ASSOC_HASH_EQ_FN_DEBUG
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_HASH_EQ_FN_DEBUG
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_HASH_EQ_FN_DEBUG

    template<typename Key, class Eq_Fn, class Allocator, bool Store_Hash>
    struct hash_eq_fn;

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Key, class Eq_Fn, class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	hash_eq_fn< \
		Key, \
		Eq_Fn, \
		Allocator, \
		false>

    /**
     * Specialization 1- The client requests that hash values not be stored.
     **/
    template<typename Key, class Eq_Fn, class Allocator>
    struct hash_eq_fn<Key, Eq_Fn, Allocator, false> : public Eq_Fn
    {
      typedef Eq_Fn my_eq_fn_base;

      typedef typename Allocator::template rebind<Key>::other key_allocator;

      typedef typename key_allocator::const_reference const_key_reference;

      hash_eq_fn();

      hash_eq_fn(const Eq_Fn& r_eq_fn);

      inline bool
      operator()(const_key_reference r_lhs_key, const_key_reference r_rhs_key) const;

      inline void
      swap(const PB_ASSOC_CLASS_C_DEC& r_other);
    };

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    hash_eq_fn()
    { }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    swap(const PB_ASSOC_CLASS_C_DEC& r_other)
    {
      std::swap((Eq_Fn& )(*this), (Eq_Fn& )r_other);
    }

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    hash_eq_fn(const Eq_Fn& r_eq_fn) :
      Eq_Fn(r_eq_fn)
    { }

    PB_ASSOC_CLASS_T_DEC
    inline bool
    PB_ASSOC_CLASS_C_DEC::
    operator()(const_key_reference r_lhs_key, const_key_reference r_rhs_key) const
    {
      return (my_eq_fn_base::operator()(r_lhs_key, r_rhs_key));
    }

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Key, class Eq_Fn, class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	hash_eq_fn< \
		Key, \
		Eq_Fn, \
		Allocator, \
		true>

    /**
     * Specialization 2- The client requests that hash values be stored.
     **/
    template<typename Key, class Eq_Fn, class Allocator>
    struct hash_eq_fn<Key, Eq_Fn, Allocator, true> :
      public Eq_Fn
    {
      typedef typename Allocator::size_type size_type;

      typedef Eq_Fn my_eq_fn_base;

      typedef typename Allocator::template rebind<Key>::other key_allocator;

      typedef typename key_allocator::const_reference const_key_reference;

      hash_eq_fn();

      hash_eq_fn(const Eq_Fn& r_eq_fn);

      inline bool
      operator()(const_key_reference r_lhs_key, size_type lhs_hash, const_key_reference r_rhs_key, size_type rhs_hash) const;

      inline void
      swap(const PB_ASSOC_CLASS_C_DEC& r_other);
    };

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    hash_eq_fn()
    { }

    PB_ASSOC_CLASS_T_DEC
    PB_ASSOC_CLASS_C_DEC::
    hash_eq_fn(const Eq_Fn& r_eq_fn) :
      Eq_Fn(r_eq_fn)
    { }

    PB_ASSOC_CLASS_T_DEC
    inline bool
    PB_ASSOC_CLASS_C_DEC::
    operator()(const_key_reference r_lhs_key, size_type lhs_hash, const_key_reference r_rhs_key, size_type rhs_hash) const
    {
      PB_ASSOC_DBG_ASSERT(!my_eq_fn_base::operator()(r_lhs_key, r_rhs_key) ||
			  lhs_hash == rhs_hash);

      return (lhs_hash == rhs_hash&& 
	      my_eq_fn_base::operator()(r_lhs_key, r_rhs_key));
    }

    PB_ASSOC_CLASS_T_DEC
    inline void
    PB_ASSOC_CLASS_C_DEC::
    swap(const PB_ASSOC_CLASS_C_DEC& r_other)
    {
      std::swap((Eq_Fn& )(*this), (Eq_Fn& )(r_other));
    }

#undef PB_ASSOC_CLASS_T_DEC
#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

  } // namespace detail

} // namespace pb_assoc

#endif // #ifndef HASH_EQ_FN_HPP
