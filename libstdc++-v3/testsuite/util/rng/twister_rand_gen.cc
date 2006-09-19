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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

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
 * @file twister_rand_gen.cc
 */

#include <ctime>
#include <debug/debug.h>
#include <util/rng/twister_rand_gen.hpp>

namespace pb_ds
{
  namespace test
  {
    twister_rand_gen::
    twister_rand_gen(unsigned int seed)
    : m_base_generator(seed)
    {
      // Do nothing.
    }

    void
    twister_rand_gen::
    init(unsigned int seed)
    { m_base_generator.seed(seed); }

    unsigned long
    twister_rand_gen::
    get_unsigned_long(unsigned long min, unsigned long max)
    {
      _GLIBCXX_DEBUG_ASSERT(max >= min);
      const double prob = get_prob();
      const unsigned long rand_word = 
	(unsigned long)((max - min + 1) * prob) + min;

      _GLIBCXX_DEBUG_ASSERT(rand_word <= max);
      return rand_word;
    }

    double
    twister_rand_gen::
    get_prob()
    {
      const double eng_min = m_base_generator.min();
      const double eng_range =
	static_cast<const double>(m_base_generator.max() - eng_min);

      const double eng_res =
	static_cast<const double>(m_base_generator() - eng_min);

      const double ret = eng_res / eng_range;
      _GLIBCXX_DEBUG_ASSERT(ret >= 0 && ret <= 1);
      return ret;
    }
  } // namespace test
} // namespace pb_ds
