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
 * @file ranged_probe_fn.hpp
 * Contains a unified ranged probe functor, allowing the probe tables to deal with
 *    a single class for ranged probeing.
 */

#ifndef PB_DS_RANGED_PROBE_FN_HPP
#define PB_DS_RANGED_PROBE_FN_HPP

#include <ext/pb_ds/detail/basic_types.hpp>
#include <utility>

namespace pb_ds
{
  namespace detail
  {

#ifdef PB_DS_RANGED_PROBE_FN_DEBUG
#define PB_DS_DBG_ASSERT(X) assert(X)
#define PB_DS_DBG_VERIFY(X) assert(X)
#define PB_DS_DBG_ONLY(X) X
#else // #ifdef PB_DS_RANGED_PROBE_FN_DEBUG
#define PB_DS_DBG_ASSERT(X)
#define PB_DS_DBG_VERIFY(X) {if((X)==0);}
#define PB_DS_DBG_ONLY(X) ;
#endif // #ifdef PB_DS_RANGED_PROBE_FN_DEBUG

    template<typename Key,
	     class Hash_Fn,
	     class Allocator,
	     class Comb_Probe_Fn,
	     class Probe_Fn,
	     bool Store_Hash>
    class ranged_probe_fn;

#define PB_DS_CLASS_T_DEC						\
    template<								\
						typename Key,		\
						class Hash_Fn,		\
						class Allocator,	\
						class Comb_Probe_Fn,	\
						class Probe_Fn>

#define PB_DS_CLASS_C_DEC					\
    ranged_probe_fn<						\
						Key,		\
						Hash_Fn,	\
						Allocator,	\
						Comb_Probe_Fn,	\
						Probe_Fn,	\
						false>

    /**
     * Specialization 1- The client supplies a probe function and a ranged
     *    probe function, and requests that hash values not be stored.
     **/
    template<typename Key,
	     class Hash_Fn,
	     class Allocator,
	     class Comb_Probe_Fn,
	     class Probe_Fn>
    class ranged_probe_fn<
      Key,
      Hash_Fn,
      Allocator,
      Comb_Probe_Fn,
      Probe_Fn,
      false> : public Hash_Fn,
	       public Comb_Probe_Fn,
	       public Probe_Fn
    {
    protected:
      typedef typename Allocator::size_type size_type;

      typedef Comb_Probe_Fn comb_probe_fn_base;

      typedef Hash_Fn hash_fn_base;

      typedef Probe_Fn probe_fn_base;

      typedef typename Allocator::template rebind<Key>::other key_allocator;

      typedef typename key_allocator::const_reference const_key_reference;

    protected:
      ranged_probe_fn(size_type size);

      ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn);

      ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn);

      ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn);

      void
      swap(PB_DS_CLASS_C_DEC& other);

      void
      notify_resized(size_type size);

      inline size_type
      operator()(const_key_reference r_key) const;

      inline size_type
      operator()(const_key_reference r_key, size_type hash, size_type i) const;
    };

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size)
    {
      Comb_Probe_Fn::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn) :
      Hash_Fn(r_hash_fn)
    {
      Comb_Probe_Fn::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn) :
      Hash_Fn(r_hash_fn),
      Comb_Probe_Fn(r_comb_probe_fn)
    {
      comb_probe_fn_base::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn) :
      Hash_Fn(r_hash_fn),
      Comb_Probe_Fn(r_comb_probe_fn),
      Probe_Fn(r_probe_fn)
    {
      comb_probe_fn_base::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    swap(PB_DS_CLASS_C_DEC& other)
    {
      comb_probe_fn_base::swap(other);

      std::swap((Hash_Fn& )(*this), (Hash_Fn& )other);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    notify_resized(size_type size)
    {
      comb_probe_fn_base::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    operator()(const_key_reference r_key) const
    {
      return (comb_probe_fn_base::operator()(
					     hash_fn_base::operator()(r_key)));
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    operator()(const_key_reference /*r_key*/, size_type hash, size_type i) const
    {
      return (comb_probe_fn_base::operator()(
					     hash + probe_fn_base::operator()(i)));
    }

#undef PB_DS_CLASS_T_DEC
#undef PB_DS_CLASS_C_DEC

#define PB_DS_CLASS_T_DEC						\
    template<								\
						typename Key,		\
						class Hash_Fn,		\
						class Allocator,	\
						class Comb_Probe_Fn,	\
						class Probe_Fn>

#define PB_DS_CLASS_C_DEC					\
    ranged_probe_fn<						\
						Key,		\
						Hash_Fn,	\
						Allocator,	\
						Comb_Probe_Fn,	\
						Probe_Fn,	\
						true>

    /**
     * Specialization 2- The client supplies a probe function and a ranged
     *    probe function, and requests that hash values not be stored.
     **/
    template<typename Key,
	     class Hash_Fn,
	     class Allocator,
	     class Comb_Probe_Fn,
	     class Probe_Fn>
    class ranged_probe_fn<
      Key,
      Hash_Fn,
      Allocator,
      Comb_Probe_Fn,
      Probe_Fn,
      true> :
      public Hash_Fn,
      public Comb_Probe_Fn,
      public Probe_Fn
    {
    protected:
      typedef typename Allocator::size_type size_type;

      typedef typename comp_hash_<size_type>::comp_hash comp_hash;

      typedef Comb_Probe_Fn comb_probe_fn_base;

      typedef Hash_Fn hash_fn_base;

      typedef Probe_Fn probe_fn_base;

      typedef typename Allocator::template rebind<Key>::other key_allocator;

      typedef typename key_allocator::const_reference const_key_reference;

    protected:
      ranged_probe_fn(size_type size);

      ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn);

      ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn);

      ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn);

      void
      swap(PB_DS_CLASS_C_DEC& other);

      void
      notify_resized(size_type size);

      inline comp_hash
      operator()(const_key_reference r_key) const;

      inline size_type
      operator()(const_key_reference r_key, size_type hash, size_type i) const;

      inline size_type
      operator()(const_key_reference r_key, size_type hash) const;
    };

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size)
    {
      Comb_Probe_Fn::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn) :
      Hash_Fn(r_hash_fn)
    {
      Comb_Probe_Fn::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn) :
      Hash_Fn(r_hash_fn),
      Comb_Probe_Fn(r_comb_probe_fn)
    {
      comb_probe_fn_base::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const Hash_Fn& r_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn, const Probe_Fn& r_probe_fn) :
      Hash_Fn(r_hash_fn),
      Comb_Probe_Fn(r_comb_probe_fn),
      Probe_Fn(r_probe_fn)
    {
      comb_probe_fn_base::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    swap(PB_DS_CLASS_C_DEC& other)
    {
      comb_probe_fn_base::swap(other);

      std::swap((Hash_Fn& )(*this), (Hash_Fn& )other);
    }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    notify_resized(size_type size)
    {
      comb_probe_fn_base::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::comp_hash
    PB_DS_CLASS_C_DEC::
    operator()(const_key_reference r_key) const
    {
      const size_type hash = hash_fn_base::operator()(r_key);

      return (std::make_pair(comb_probe_fn_base::operator()(hash), hash));
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    operator()(const_key_reference /*r_key*/, size_type hash, size_type i) const
    {
      return (comb_probe_fn_base::operator()(
					     hash + probe_fn_base::operator()(i)));
    }

    PB_DS_CLASS_T_DEC
    inline typename PB_DS_CLASS_C_DEC::size_type
    PB_DS_CLASS_C_DEC::
    operator()
#ifdef PB_DS_RANGED_PROBE_FN_DEBUG
      (const_key_reference r_key, size_type hash) const
#else // #ifdef PB_DS_RANGED_PROBE_FN_DEBUG
      (const_key_reference /*r_key*/, size_type hash) const
#endif // #ifdef PB_DS_RANGED_PROBE_FN_DEBUG
    {
      PB_DS_DBG_ASSERT(hash == hash_fn_base::operator()(r_key));

      return (hash);
    }

#undef PB_DS_CLASS_T_DEC
#undef PB_DS_CLASS_C_DEC

#define PB_DS_CLASS_T_DEC						\
    template<typename Key, class Allocator, class Comb_Probe_Fn>

#define PB_DS_CLASS_C_DEC					\
    ranged_probe_fn<						\
						Key,		\
						null_hash_fn,	\
						Allocator,	\
						Comb_Probe_Fn,	\
						null_probe_fn,	\
						false>

    /**
     * Specialization 3 and 4- The client does not supply a hash function or
     *    probe function, and requests that hash values not be stored.
     **/
    template<typename Key, class Allocator, class Comb_Probe_Fn>
    class ranged_probe_fn<
      Key,
      null_hash_fn,
      Allocator,
      Comb_Probe_Fn,
      null_probe_fn,
      false> :
      public Comb_Probe_Fn,
      public null_hash_fn,
      public null_probe_fn
    {
    protected:
      typedef typename Allocator::size_type size_type;

      typedef Comb_Probe_Fn comb_probe_fn_base;

      typedef typename Allocator::template rebind<Key>::other key_allocator;

      typedef typename key_allocator::const_reference const_key_reference;

    protected:
      ranged_probe_fn(size_type size);

      ranged_probe_fn(size_type size, const Comb_Probe_Fn& r_comb_probe_fn);

      ranged_probe_fn(size_type size, const null_hash_fn& r_null_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn, const null_probe_fn& r_null_probe_fn);

      void
      swap(PB_DS_CLASS_C_DEC& other);
    };

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size)
    {
      Comb_Probe_Fn::notify_resized(size);
    }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const Comb_Probe_Fn& r_comb_probe_fn) :
      Comb_Probe_Fn(r_comb_probe_fn)
    { }

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    ranged_probe_fn(size_type size, const null_hash_fn& r_null_hash_fn, const Comb_Probe_Fn& r_comb_probe_fn, const null_probe_fn& r_null_probe_fn) :
      Comb_Probe_Fn(r_comb_probe_fn)
    { }

    PB_DS_CLASS_T_DEC
    void
    PB_DS_CLASS_C_DEC::
    swap(PB_DS_CLASS_C_DEC& other)
    {
      comb_probe_fn_base::swap(other);
    }

#undef PB_DS_CLASS_T_DEC
#undef PB_DS_CLASS_C_DEC

#undef PB_DS_DBG_ASSERT
#undef PB_DS_DBG_VERIFY
#undef PB_DS_DBG_ONLY

  } // namespace detail
} // namespace pb_ds

#endif // #ifndef PB_DS_RANGED_PROBE_FN_HPP

