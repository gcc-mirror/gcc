// -*- C++ -*-

// Copyright (C) 2005-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


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
 * @file native_hash_set.hpp
 * Contains an adapter to TR1 unordered containers.
 */

#ifndef PB_DS_NATIVE_HASH_SET_HPP
#define PB_DS_NATIVE_HASH_SET_HPP

#include <string>
#include <ext/pb_ds/detail/type_utils.hpp>
#include <ext/pb_ds/detail/standard_policies.hpp>
#include <native_type/native_hash_tag.hpp>
#include <io/xml.hpp>
#include <tr1/unordered_set>

namespace __gnu_pbds
{
  namespace test
  {
#define PB_DS_BASE_C_DEC \
    std::tr1::__unordered_set<Key, Hash_Fn, Eq_Fn, \
    typename _Alloc::template rebind<Key>::other, Cache_Hash>

    template<typename Key,
	     size_t Init_Size = 8,
         typename Hash_Fn = typename __gnu_pbds::detail::default_hash_fn<Key>::type,
	     typename Eq_Fn = std::equal_to<Key>,
	     typename Less_Fn = std::less<Key>,
	     typename _Alloc = std::allocator<char>, bool Cache_Hash = false
	     >
    class native_hash_set : public PB_DS_BASE_C_DEC
    {
    private:
      typedef PB_DS_BASE_C_DEC base_type;

    public:
      typedef native_hash_tag container_category;

      native_hash_set() : base_type(Init_Size) { }

      template<typename It>
      native_hash_set(It f, It l) : base_type(f, l) { }

      template<typename Pred>
      inline size_t
      erase_if(Pred pred)
      {
        size_t ersd = 0;
        bool done = false;
        while (!done)
	  {
            typename base_type::iterator b_it = base_type::begin();
            typename base_type::iterator e_it = base_type::end();
            done = true;
            while (b_it != e_it)
	      {
                if (pred(*b_it))
		  {
                    ++ersd;
                    done = false;
                    base_type::erase(*b_it);
                    b_it = e_it;
		  }
                else
		  ++b_it;
	      }
	  }
        return ersd;
      }

      static std::string
      name()
      {
        return std::string("n_hash_set_") 
	       + (Cache_Hash ? std::string("cah") : std::string("ncah"));
      }

      static std::string
      desc()
      {
        const std::string cache_hash_desc =
	make_xml_tag("cache_hash_code", "value",
		     Cache_Hash ? std::string("true") : std::string("false"));

        return make_xml_tag("type", "value", "std_tr1_unordered_set", 
			    cache_hash_desc);
      }
    };

#undef PB_DS_BASE_C_DEC
  } // namespace test
} // namespace __gnu_pbds

#endif
