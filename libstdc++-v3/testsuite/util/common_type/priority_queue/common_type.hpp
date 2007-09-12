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
 * @file common_type.hpp
 * Contains common types.
 */

#ifndef PB_DS_COMMON_TYPES_HPP
#define PB_DS_COMMON_TYPES_HPP

#include <ext/pb_ds/detail/type_utils.hpp>
#include <ext/pb_ds/priority_queue.hpp>
#include <ext/typelist.h>

namespace __gnu_pbds
{
  namespace test
  {
    template<typename Value_Type, typename Cmp_Fn = std::less<Value_Type>,
	     class Allocator = std::allocator<Value_Type> >
    struct pq_common_types
    {
    private:
      //    typedef typename Allocator::size_type size_type;

      typedef __gnu_pbds::priority_queue<Value_Type, Cmp_Fn, __gnu_pbds::pairing_heap_tag, Allocator> pairing_heap_t;

      typedef __gnu_pbds::priority_queue<Value_Type, Cmp_Fn, __gnu_pbds::binomial_heap_tag, Allocator> binomial_heap_t;

      typedef __gnu_pbds::priority_queue<Value_Type, Cmp_Fn, __gnu_pbds::rc_binomial_heap_tag, Allocator> rc_binomial_heap_t;

      typedef __gnu_pbds::priority_queue<Value_Type, Cmp_Fn, __gnu_pbds::binary_heap_tag, Allocator> binary_heap_t;

      typedef __gnu_pbds::priority_queue<Value_Type, Cmp_Fn, __gnu_pbds::thin_heap_tag, Allocator> thin_heap_t;

      typedef typename __gnu_cxx::typelist::create5<thin_heap_t, pairing_heap_t, binomial_heap_t, rc_binomial_heap_t, binary_heap_t>::type all_tl;

    public:
      typedef all_tl performance_tl;
      typedef all_tl regression_tl;
      typedef all_tl performance_min_tl;
      typedef all_tl regression_min_tl;
    };

  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef PB_DS_COMMON_TYPES_HPP
