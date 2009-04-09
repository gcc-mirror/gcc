// -*- C++ -*-

// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file parallel/random_number.h
 *  @brief Random number generator based on the Mersenne twister.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_RANDOM_NUMBER_H
#define _GLIBCXX_PARALLEL_RANDOM_NUMBER_H 1

#include <parallel/types.h>
#include <tr1/random>

namespace __gnu_parallel
{
  /** @brief Random number generator, based on the Mersenne twister. */
  class random_number
  {
  private:
    std::tr1::mt19937 	mt;
    uint64 		supremum;
    uint64 		RAND_SUP;
    double 		supremum_reciprocal;
    double 		RAND_SUP_REC;

    // Assumed to be twice as long as the usual random number.
    uint64 		cache;  

    // Bit results.
    int bits_left;
    
    static uint32
    scale_down(uint64 x,
#if _GLIBCXX_SCALE_DOWN_FPU
	       uint64 /*supremum*/, double supremum_reciprocal)
#else
               uint64 supremum, double /*supremum_reciprocal*/)
#endif
	{
#if _GLIBCXX_SCALE_DOWN_FPU
	  return uint32(x * supremum_reciprocal);
#else
	  return static_cast<uint32>(x % supremum);
#endif
	}

  public:
    /** @brief Default constructor. Seed with 0. */
    random_number()
    : mt(0), supremum(0x100000000ULL),
      RAND_SUP(1ULL << (sizeof(uint32) * 8)),
      supremum_reciprocal(double(supremum) / double(RAND_SUP)),
      RAND_SUP_REC(1.0 / double(RAND_SUP)),
      cache(0), bits_left(0) { }

    /** @brief Constructor.
     *  @param seed Random seed.
     *  @param supremum Generate integer random numbers in the
     *                  interval @c [0,supremum). */
    random_number(uint32 seed, uint64 supremum = 0x100000000ULL)
    : mt(seed), supremum(supremum),
      RAND_SUP(1ULL << (sizeof(uint32) * 8)),
      supremum_reciprocal(double(supremum) / double(RAND_SUP)),
      RAND_SUP_REC(1.0 / double(RAND_SUP)),
      cache(0), bits_left(0) { }

    /** @brief Generate unsigned random 32-bit integer. */
    uint32
    operator()()
    { return scale_down(mt(), supremum, supremum_reciprocal); }

    /** @brief Generate unsigned random 32-bit integer in the
	interval @c [0,local_supremum). */
    uint32
    operator()(uint64 local_supremum)
    {
      return scale_down(mt(), local_supremum,
			double(local_supremum * RAND_SUP_REC));
    }

    /** @brief Generate a number of random bits, run-time parameter.
     *  @param bits Number of bits to generate. */
    unsigned long
    genrand_bits(int bits)
    {
      unsigned long res = cache & ((1 << bits) - 1);
      cache = cache >> bits;
      bits_left -= bits;
      if (bits_left < 32)
	{
	  cache |= ((uint64(mt())) << bits_left);
	  bits_left += 32;
	}
      return res;
    }
};

} // namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_RANDOM_NUMBER_H */
