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
 * @file native_hash_multimap.hpp
 * Contains an adapter to Dinkumware/SGI hash tables
 */

#ifndef PB_DS_NATIVE_HASH_MULTIMAP_HPP
#define PB_DS_NATIVE_HASH_MULTIMAP_HPP

#include <ext/pb_ds/detail/standard_policies.hpp>
#include <native_type/assoc/native_hash_tag.hpp>
#include <io/xml.hpp>
#include <string>
#include <ext/hash_map>

namespace pb_ds
{

  namespace test
  {

#ifdef NATIVE_HASH_MULTIMAP_DEBUG
#define PB_DS_DBG_ASSERT(X) assert(X)
#define PB_DS_DBG_VERIFY(X) assert(X)
#define PB_DS_DBG_ONLY(X) X
#else // #ifdef NATIVE_HASH_MULTIMAP_DEBUG
#define PB_DS_DBG_ASSERT(X)
#define PB_DS_DBG_VERIFY(X) {if((X)==0);}
#define PB_DS_DBG_ONLY(X) ;
#endif // #ifdef NATIVE_HASH_MULTIMAP_DEBUG

#define PB_DS_CLASS_T_DEC						\
    template<								\
						typename Key,		\
						typename Data,		\
						size_t Init_Size,	\
						class Hash_Fn,		\
						class Eq_Fn,		\
						class Less_Fn,		\
						class Allocator>

#define PB_DS_CLASS_C_DEC					\
    native_hash_multimap<					\
						Key,		\
						Data,		\
						Init_Size,	\
						Hash_Fn,	\
						Eq_Fn,		\
						Less_Fn,	\
						Allocator>

#define PB_DS_BASE_C_DEC					\
    __gnu_cxx::hash_multimap<					\
						Key,		\
						Data,		\
						Hash_Fn,	\
						Eq_Fn,		\
						Allocator>

    template<typename Key,
	     typename Data,
	     size_t Init_Size = 8,
	     class Hash_Fn =
	     typename pb_ds::detail::default_hash_fn<Key>::type,
	     class Eq_Fn =
	     std::equal_to<Key>,
	     class Less_Fn =
	     std::less<Key>,
	     class Allocator =
	     std::allocator<char> >
    class native_hash_multimap : public PB_DS_BASE_C_DEC
    {
    private:
      typedef PB_DS_BASE_C_DEC base_type;

    public:
      typedef native_hash_tag container_category;

      typedef Allocator allocator;

      typedef typename base_type::iterator iterator;

      typedef typename base_type::const_iterator const_iterator;

      typedef
      typename Allocator::template rebind<
	std::pair<
	Key,
	Data> >::other::const_reference
      const_reference;

    public:
      native_hash_multimap();

      inline void
      insert(typename base_type::const_reference r_val)
      {
        typedef
	  std::pair<
	  typename base_type::iterator,
	  typename base_type::iterator>
	  eq_range_t;

        eq_range_t f = base_type::equal_range(r_val.first);

        typename base_type::iterator it = f.first;

        while (it != f.second)
	  {
            if (it->second == r_val.second)
	      return;

            ++it;
	  }

        base_type::insert(r_val);
      }

      inline iterator
      find(const_reference r_val)
      {
        typedef
	std::pair<
	typename base_type::iterator,
	typename base_type::iterator>
	eq_range_t;

        eq_range_t f = base_type::equal_range(r_val.first);

        typename base_type::iterator it = f.first;

        while (it != f.second)
	  {
            if (it->second == r_val.second)
	      return it;

            ++it;
	  }

        return base_type::end();
      }

      inline const_iterator
      find(const_reference r_val) const
      {
        typedef
	std::pair<
	typename base_type::const_iterator,
	typename base_type::const_iterator>
	eq_range_t;

        eq_range_t f = base_type::equal_range(r_val.first);

        typename base_type::const_iterator it = f.first;

        while (it != f.second)
	  {
            if (it->second == r_val.second)
	      return it;

            ++it;
	  }

        return base_type::end();
      }

      template<typename It>
      native_hash_multimap(It f, It l);

      static std::string
      name()
      {
        return ("n_hash_mmap");
      }

      static std::string
      desc()
      {
        return (make_xml_tag(            "type", "value", "__gnucxx_hash_multimap"));
      }
    };

    PB_DS_CLASS_T_DEC
    PB_DS_CLASS_C_DEC::
    native_hash_multimap()
#ifdef __GNUC__
      :
    base_type(Init_Size)
#endif // #ifdef __GNUC__
    { }

    PB_DS_CLASS_T_DEC
    template<typename It>
    PB_DS_CLASS_C_DEC::
    native_hash_multimap(It f, It l) :
      base_type(f, l)
    { }

#undef PB_DS_CLASS_T_DEC

#undef PB_DS_CLASS_C_DEC

#undef PB_DS_BASE_C_DEC

#undef PB_DS_DBG_ASSERT
#undef PB_DS_DBG_VERIFY
#undef PB_DS_DBG_ONLY

  } // namespace test

} // namespace pb_ds

#endif // #ifndef PB_DS_NATIVE_HASH_MULTIMAP_HPP
