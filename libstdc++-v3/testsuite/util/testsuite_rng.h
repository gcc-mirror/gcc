// -*- C++ -*-

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
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
 * @file testsuite_rng.h
 */

#ifndef _GLIBCXX_TESTSUITE_RNG_H
#define _GLIBCXX_TESTSUITE_RNG_H

#include <ctime>
#include <climits>
#include <debug/debug.h>
#include <tr1/random>

namespace __gnu_pbds
{
  namespace test
  {
    class twister_rand_gen
    {
    public:
      twister_rand_gen(unsigned int seed =
		       static_cast<unsigned int>(std::time(0)))
      : m_base_generator(seed)
      {
	// Do nothing.
      }

      void
      init(unsigned int seed)
      { m_base_generator.seed(seed); }

      static unsigned int
      get_time_determined_seed()
      { return(static_cast<unsigned int>(std::time(0))); }

      unsigned long
      get_unsigned_long(unsigned long min = 0,
			unsigned long max = UINT_MAX - 1)
      {
	_GLIBCXX_DEBUG_ASSERT(max >= min);
	const double prob = get_prob();
	const unsigned long r = (unsigned long)((max - min + 1) * prob) + min;
	_GLIBCXX_DEBUG_ASSERT(r <= max);
	return r;
      }

      double
      get_prob()
      {
	const double min = m_base_generator.min();
	const double max = m_base_generator.max();
	const double range = static_cast<double>(max - min);
	const double res = static_cast<double>(m_base_generator() - min);
	const double ret = res / range;
	_GLIBCXX_DEBUG_ASSERT(ret >= 0 && ret <= 1);
	return ret;
      }

    private:
      typedef std::tr1::mt19937 base_generator_t;

      base_generator_t m_base_generator;
    };
  } // namespace test
} // namespace __gnu_pbds

#endif // #ifndef _GLIBCXX_TESTSUITE_RNG_H
