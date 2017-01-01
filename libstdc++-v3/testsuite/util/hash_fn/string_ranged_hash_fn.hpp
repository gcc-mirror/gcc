// -*- C++ -*-

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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
 * @file string_ranged_hash_fn.hpp
 * Contains a ranged string hash function.
 */

#ifndef PB_DS_STRING_RANGED_HASH_FN_HPP
#define PB_DS_STRING_RANGED_HASH_FN_HPP

#include <string>
#include <hash_fn/dna_str_limit.hpp>

namespace __gnu_pbds
{
  namespace test
  {
    template<typename Comb_Fn>
    class string_ranged_hash_fn : public Comb_Fn
    {
    private:
      size_t _M_limit;

    public:
      typedef Comb_Fn comb_fn;
      typedef detail::comb_hash_fn_string_form<comb_fn> string_form;

      inline size_t
      operator()(const std::string& r_str) const
      {
	size_t ret = 0;
	std::string::const_iterator b = r_str.begin();
	std::string::const_iterator e = r_str.end();

	size_t i = 0;
	while (i < _M_limit&&  b != e)
	  {
	    ret *= 5;
	    ret += static_cast<size_t>(*(b++));
	    ++i;
	  }
	return (comb_fn::operator()(ret));
      }

      void
      swap(string_ranged_hash_fn<Comb_Fn>& other)
      {
	std::swap(_M_limit, other._M_limit);
	comb_fn::swap(other);
      }

      void
      notify_resized(size_t new_size)
      {
	comb_fn::notify_resized(new_size);
	_M_limit = dna_str_limit(new_size);
      }

      static std::string
      name()
      { return ("string_ranged_hash_fn_" + string_form<comb_fn>::name()); }

      static std::string
      desc()
      { return ("string ranged-hash using" + string_form<comb_fn>::desc()); }
    };
  } // namespace test
} // namespace __gnu_pbds

#endif
