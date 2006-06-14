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
 * @file twister_rand_gen.cpp
 * Contains a random number generator invented and implemented by
 *    Makoto Matsumoto
 *    (http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html)
 */

#include <util/rng/twister_rand_gen.hpp>
#include <ctime>
#include <iostream>

namespace pb_ds
{
  namespace test
  {
#ifdef TWISTER_RAND_GEN_DEBUG
#define PB_DS_DBG_ASSERT(X) assert(X)
#define PB_DS_DBG_VERIFY(X) assert(X)
#define PB_DS_DBG_ONLY(X) X
#else // #ifdef TWISTER_RAND_GEN_DEBUG
#define PB_DS_DBG_ASSERT(X)
#define PB_DS_DBG_VERIFY(X) {if((X)==0);}
#define PB_DS_DBG_ONLY(X) ;
#endif // #ifdef TWISTER_RAND_GEN_DEBUG

    enum
      {
	mers_n = 624,
	mers_m = 397,
	mers_r = 31,
	mers_u = 11,
	mers_s = 7,
	mers_t = 15,
	mers_l = 18,
	mers_a = 0x9908B0DF,
	mers_b = 0x9D2C5680,
	mers_c = 0xEFC60000
      };

    twister_rand_gen::
    twister_rand_gen(unsigned long seed)
    { init(seed); }

    void
    twister_rand_gen::
    init(unsigned long seed)
    {
      m_a_mt[0]= seed;

      for (m_mti=1; m_mti < mers_n; ++m_mti)
	m_a_mt[m_mti] = (1812433253UL* (m_a_mt[m_mti-1] ^ (m_a_mt[m_mti-1] >> 30)) + m_mti);

      union
      {
	double m_f;
	unsigned long m_a[2];
      } u;

      u.m_f = 1.0;

      if (u.m_a[1] == 0x3FF00000)
	m_endianess = little;
      else if (u.m_a[0] == 0x3FF00000)
	m_endianess = big;
      else
	m_endianess = none;
    }

    unsigned long
    twister_rand_gen::
    get_unsigned_long(unsigned long min, unsigned long max)
    {
      PB_DS_DBG_ASSERT(max >= min);
      const double prob = get_prob();
      const unsigned long rand_word =(unsigned long)((max - min+ 1)*  prob) + min;

      PB_DS_DBG_ASSERT(rand_word <= max);
      return rand_word;
    }

    double
    twister_rand_gen::
    get_prob()
    {
      union
      {
	double m_f;
	unsigned long m_a[2];
      } u;

      unsigned long rand_word = get_unsigned_long_imp();

      double ret;

      switch(m_endianess)
	{
	case little:
	  u.m_a[0] =    rand_word << 20;
	  u.m_a[1] = (rand_word >> 12) | 0x3FF00000;
	  ret = u.m_f - 1.0;
	  break;
	case big:
	  u.m_a[1] =    rand_word << 20;
	  u.m_a[0] = (rand_word >> 12) | 0x3FF00000;
	  ret = u.m_f - 1.0;
	  break;
	case none:
	default:
	  break;
	}

      ret = (double)rand_word * (1./((double)(unsigned long)(-1L)+1.));

      PB_DS_DBG_ASSERT(ret >= 0);
      PB_DS_DBG_ASSERT(ret <= 1);

      return ret;
    }

    unsigned long
    twister_rand_gen::
    get_unsigned_long_imp()
    {
      unsigned long y;
      if (m_mti >= mers_n)
	{
	  const unsigned long LOWER_MASK = (1LU << mers_r) - 1;
	  const unsigned long UPPER_MASK = static_cast<unsigned long>(-1L << mers_r);
	  static const unsigned long m_a_mag01[2] = {0, mers_a};

	  unsigned long kk;
	  for (kk=0; kk < mers_n-mers_m; ++kk)
	    {
	      y = (m_a_mt[kk]&  UPPER_MASK) | (m_a_mt[kk+1]&  LOWER_MASK);
	      m_a_mt[kk] = m_a_mt[kk+mers_m] ^ (y >> 1) ^ m_a_mag01[y&  1];
	    }

	  for (; kk < mers_n-1; ++kk)
	    {
	      y = (m_a_mt[kk]&  UPPER_MASK) | (m_a_mt[kk+1]&  LOWER_MASK);
	      m_a_mt[kk] = m_a_mt[kk+(mers_m-mers_n)] ^ (y >> 1) ^ m_a_mag01[y&  1];
	    }

	  y = (m_a_mt[mers_n-1]&  UPPER_MASK) | (m_a_mt[0]&  LOWER_MASK);
	  m_a_mt[mers_n-1] = m_a_mt[mers_m-1] ^ (y >> 1) ^ m_a_mag01[y&  1];
	  m_mti = 0;
	}

      y = m_a_mt[m_mti++];

      y ^=    y >> mers_u;
      y ^= (y << mers_s)&  mers_b;
      y ^= (y << mers_t)&  mers_c;
      y ^=    y >> mers_l;
      return y;
    }

#undef PB_DS_DBG_ASSERT
#undef PB_DS_DBG_VERIFY
#undef PB_DS_DBG_ONLY
  } // namespace test
} // namespace pb_ds
