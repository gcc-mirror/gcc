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

#include <string>
#include <ext/hash_map>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <ext/pb_ds/detail/standard_policies.hpp>
#include <native_type/assoc/native_hash_tag.hpp>
#include <io/xml.hpp>

namespace pb_ds
{
  namespace test
  {
#define PB_DS_BASE_C_DEC \
    __gnu_cxx::hash_multimap<Key, Data, Hash_Fn, Eq_Fn, Allocator>

    template<typename Key,
	     typename Data,
	     size_t Init_Size = 8,
	     class Hash_Fn = typename pb_ds::detail::default_hash_fn<Key>::type,
	     class Eq_Fn = std::equal_to<Key>,
	     class Less_Fn = std::less<Key>,
	     class Allocator = std::allocator<char> >
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
	std::pair<Key, Data> >::other::const_reference
      const_reference;

      native_hash_multimap() : base_type(Init_Size)
      { }

      template<typename It>
      native_hash_multimap(It f, It l) : base_type(f, l)
      { }

      inline void
      insert(typename base_type::const_reference r_val)
      {
        typedef std::pair<iterator, iterator> eq_range_t;
        eq_range_t f = base_type::equal_range(r_val.first);

        iterator it = f.first;
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
        typedef std::pair<iterator, iterator> eq_range_t;
        eq_range_t f = base_type::equal_range(r_val.first);

        iterator it = f.first;
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
        typedef std::pair<const_iterator, const_iterator> eq_range_t;
        eq_range_t f = base_type::equal_range(r_val.first);

        const_iterator it = f.first;
        while (it != f.second)
	  {
            if (it->second == r_val.second)
	      return it;
            ++it;
	  }
        return base_type::end();
      }

      static std::string
      name()
      { return std::string("n_hash_mmap"); }

      static std::string
      desc()
      {
        return make_xml_tag("type", "value", "__gnucxx_hash_multimap");
      }
    };

#undef PB_DS_BASE_C_DEC

  } // namespace test
} // namespace pb_ds

#endif 
