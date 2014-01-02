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
 * @file basic_type.hpp
 * Contains a type used for regression tests' key and data.
 */

#ifndef PB_DS_BASIC_TYPE_HPP
#define PB_DS_BASIC_TYPE_HPP

#include <string>
#include <ext/throw_allocator.h>

namespace __gnu_pbds
{
namespace test
{
#define PB_DS_BASE_C_DEC \
  std::basic_string<char, std::char_traits<char>, \
		    __gnu_cxx::throw_allocator_random<char> >

  struct basic_type : public PB_DS_BASE_C_DEC
  {
  private:
    typedef PB_DS_BASE_C_DEC base_type;

  public:
    enum
      {
	distinct_chars = 4
      };

    basic_type() { }

    template<typename Gen>
    basic_type(Gen& r_gen, size_t max)
    {
      size_t rnd = r_gen.get_unsigned_long(0, static_cast<unsigned long>(max));
      while (rnd > 0)
	{
	  base_type::push_back('a' + static_cast<char>(rnd % distinct_chars));
	  rnd /= distinct_chars;
	}
    }

    basic_type(const std::string other)
    {
      std::string::const_iterator it = other.begin();
      while (it != other.end())
	{
	  base_type::push_back(*it);
	  ++it;
	}
    }

    operator std::string() const
    { return std::string(base_type::c_str());}
  };

#undef PB_DS_BASE_C_DEC

} // namespace test
} // namespace __gnu_pbds

#endif 
