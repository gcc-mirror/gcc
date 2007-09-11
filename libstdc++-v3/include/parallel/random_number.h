// -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
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

/** @file parallel/random_number.h
 *  @brief Random number generator based on the Mersenne twister.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_RANDOM_NUMBER_H
#define _GLIBCXX_PARALLEL_RANDOM_NUMBER_H 1

#include <parallel/types.h>

namespace __gnu_parallel
{
  // XXX use tr1 random number.
  // http://www.math.keio.ac.jp/matumoto/emt.html
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  class mersenne_twister
  {
  public:
    typedef UIntType result_type;
    static const int word_size = w;
    static const int state_size = n;
    static const int shift_size = m;
    static const int mask_bits = r;
    static const UIntType parameter_a = a;
    static const int output_u = u;
    static const int output_s = s;
    static const UIntType output_b = b;
    static const int output_t = t;
    static const UIntType output_c = c;
    static const int output_l = l;

    static const bool has_fixed_range = false;

    mersenne_twister() { seed(); }

#if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x520)
    // Work around overload resolution problem (Gennadiy E. Rozental)
    explicit mersenne_twister(const UIntType& value)
#else
      explicit mersenne_twister(UIntType value)
#endif
    { seed(value); }
    template<typename It> mersenne_twister(It& first, It last) { seed(first,last); }

    template<typename Generator>
    explicit mersenne_twister(Generator & gen) { seed(gen); }

    // compiler-generated copy ctor and assignment operator are fine

    void seed() { seed(UIntType(5489)); }

#if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x520)
    // Work around overload resolution problem (Gennadiy E. Rozental)
    void seed(const UIntType& value)
#else
      void seed(UIntType value)
#endif
    {
      // New seeding algorithm from
      // http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html
      // In the previous versions, MSBs of the seed affected only MSBs of the
      // state x[].
      const UIntType mask = ~0u;
      x[0] = value & mask;
      for (i = 1; i < n; i++) {
	// See Knuth "The Art of Computer Programming" Vol. 2, 3rd ed., page 106
	x[i] = (1812433253UL * (x[i-1] ^ (x[i-1] >> (w-2))) + i) & mask;
      }
    }

    // For GCC, moving this function out-of-line prevents inlining, which may
    // reduce overall object code size.  However, MSVC does not grok
    // out-of-line definitions of member function templates.
    template<typename Generator>
    void seed(Generator & gen)
    {
      // I could have used std::generate_n, but it takes "gen" by value
      for (int j = 0; j < n; j++)
	x[j] = gen();
      i = n;
    }

    template<typename It>
    void seed(It& first, It last)
    {
      int j;
      for (j = 0; j < n && first != last; ++j, ++first)
	x[j] = *first;
      i = n;
      /*    if (first == last && j < n)
	    throw std::invalid_argument("mersenne_twister::seed");*/
    }

    result_type min() const { return 0; }
    result_type max() const
    {
      // avoid "left shift count >= with of type" warning
      result_type res = 0;
      for (int i = 0; i < w; ++i)
	res |= (1u << i);
      return res;
    }

    result_type operator()();
    static bool validation(result_type v) { return val == v; }

#ifndef BOOST_NO_OPERATORS_IN_NAMESPACE

    friend bool operator==(const mersenne_twister& x, const mersenne_twister& y)
    {
      for (int j = 0; j < state_size; ++j)
	if (x.compute(j) != y.compute(j))
	  return false;
      return true;
    }

    friend bool operator!=(const mersenne_twister& x, const mersenne_twister& y)
    { return !(x == y); }
#else
    // Use a member function; Streamable concept not supported.
    bool operator==(const mersenne_twister& rhs) const
    {
      for (int j = 0; j < state_size; ++j)
	if (compute(j) != rhs.compute(j))
	  return false;
      return true;
    }

    bool operator!=(const mersenne_twister& rhs) const
    { return !(*this == rhs); }
#endif

  private:
    // returns x(i-n+index), where index is in 0..n-1
    UIntType compute(unsigned int index) const
    {
      // equivalent to (i-n+index) % 2n, but doesn't produce negative numbers
      return x[ (i + n + index) % (2*n) ];
    }
    void twist(int block);

    // state representation: next output is o(x(i))
    //   x[0]  ... x[k] x[k+1] ... x[n-1]     x[n]     ... x[2*n-1]   represents
    //  x(i-k) ... x(i) x(i+1) ... x(i-k+n-1) x(i-k-n) ... x[i(i-k-1)]
    // The goal is to always have x(i-n) ... x(i-1) available for
    // operator== and save/restore.

    UIntType x[2*n];
    int i;
  };

#ifndef BOOST_NO_INCLASS_MEMBER_INITIALIZATION
  //  A definition is required even for integral static constants
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const bool mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::has_fixed_range;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const int mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::state_size;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const int mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::shift_size;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const int mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::mask_bits;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const UIntType mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::parameter_a;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const int mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::output_u;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const int mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::output_s;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const UIntType mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::output_b;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const int mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::output_t;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const UIntType mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::output_c;
  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  const int mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::output_l;
#endif

  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  void mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::twist(int block)
  {
    const UIntType upper_mask = (~0u) << r;
    const UIntType lower_mask = ~upper_mask;

    if (block == 0) {
      for (int j = n; j < 2*n; j++) {
	UIntType y = (x[j-n] & upper_mask) | (x[j-(n-1)] & lower_mask);
	x[j] = x[j-(n-m)] ^ (y >> 1) ^ (y&1 ? a : 0);
      }
    } else if (block == 1) {
      // split loop to avoid costly modulo operations
      {  // extra scope for MSVC brokenness w.r.t. for scope
	for (int j = 0; j < n-m; j++) {
	  UIntType y = (x[j+n] & upper_mask) | (x[j+n+1] & lower_mask);
	  x[j] = x[j+n+m] ^ (y >> 1) ^ (y&1 ? a : 0);
	}
      }

      for (int j = n-m; j < n-1; j++) {
	UIntType y = (x[j+n] & upper_mask) | (x[j+n+1] & lower_mask);
	x[j] = x[j-(n-m)] ^ (y >> 1) ^ (y&1 ? a : 0);
      }
      // last iteration
      UIntType y = (x[2*n-1] & upper_mask) | (x[0] & lower_mask);
      x[n-1] = x[m-1] ^ (y >> 1) ^ (y&1 ? a : 0);
      i = 0;
    }
  }

  template<typename UIntType, int w, int n, int m, int r, UIntType a, int u,
	   int s, UIntType b, int t, UIntType c, int l, UIntType val>
  inline typename mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::result_type
  mersenne_twister<UIntType,w,n,m,r,a,u,s,b,t,c,l,val>::operator()()
  {
    if (i == n)
      twist(0);
    else if (i >= 2*n)
      twist(1);
    // Step 4
    UIntType z = x[i];
    ++i;
    z ^= (z >> u);
    z ^= ((z << s) & b);
    z ^= ((z << t) & c);
    z ^= (z >> l);
    return z;
  }


  typedef mersenne_twister<uint32,32,351,175,19,0xccab8ee7,11,
			   7,0x31b6ab00,15,0xffe50000,17, 0xa37d3c92> mt11213b;

  // validation by experiment from mt19937.c
  typedef mersenne_twister<uint32,32,624,397,31,0x9908b0df,11,
			   7,0x9d2c5680,15,0xefc60000,18, 3346425566U> mt19937;

  /** @brief Random number generator, based on the Mersenne twister. */
  class random_number
  {
  private:
    mt19937 mt;
    uint64 supremum, RAND_SUP;
    double supremum_reciprocal, RAND_SUP_REC;

    uint64 cache;	/* assumed to be twice as long as the usual random number */
    int bits_left;	/* bit results */

    static inline uint32 scale_down(uint64 x,
#if _GLIBCXX_SCALE_DOWN_FPU
				    uint64 /*supremum*/, double supremum_reciprocal)
#else
      uint64 supremum, double /*supremum_reciprocal*/)
#endif
	{
#if _GLIBCXX_SCALE_DOWN_FPU
	  return (uint32)(x * supremum_reciprocal);
#else
	  return static_cast<uint32>(x % supremum);
#endif
	}

public:
  /** @brief Default constructor. Seed with 0. */
  random_number() :
    mt(0),
    supremum(0x100000000ULL),
    RAND_SUP(1ULL << (sizeof(uint32) * 8)),
    supremum_reciprocal((double)supremum / (double)RAND_SUP),
    RAND_SUP_REC(1.0 / (double)RAND_SUP),
    cache(0), bits_left(0)
  {
  }

  /** @brief Constructor.
   *  @param seed Random seed.
   *  @param supremum Generate integer random numbers in the interval @c [0,supremum). */
  random_number(uint32 seed, uint64 supremum = 0x100000000ULL) :
    mt(seed),
    supremum(supremum),
    RAND_SUP(1ULL << (sizeof(uint32) * 8)),
    supremum_reciprocal((double)supremum / (double)RAND_SUP),
    RAND_SUP_REC(1.0 / (double)RAND_SUP),
    cache(0), bits_left(0)
  {
  }

  /** @brief Generate unsigned random 32-bit integer. */
  inline uint32 operator()()
  {
    return scale_down(mt(), supremum, supremum_reciprocal);
  }

  /** @brief Generate unsigned random 32-bit integer in the interval @c [0,local_supremum). */
  inline uint32 operator()(uint64 local_supremum)
  {
    return scale_down(mt(), local_supremum, (double)local_supremum * RAND_SUP_REC);
  }

  /** @brief Set the random seed.
   *  @param seed to set. */
  inline void set_seed(uint32 seed)
  {
    mt.seed(seed);
    cache = mt();
    bits_left = 32;
  }

  /** @brief Generate a number of random bits, compile-time parameter. */
  template<int bits>
  inline unsigned long genrand_bits()
  {
    unsigned long res = cache & ((1 << bits) - 1);
    cache = cache >> bits;
    bits_left -= bits;
    if (bits_left < 32)
      {
	cache |= (((uint64)mt()) << bits_left);
	bits_left += 32;
      }
    return res;
  }

  /** @brief Generate a number of random bits, run-time parameter.
   *  @param bits Number of bits to generate. */
  inline unsigned long genrand_bits(int bits)
  {
    unsigned long res = cache & ((1 << bits) - 1);
    cache = cache >> bits;
    bits_left -= bits;
    if (bits_left < 32)
      {
	cache |= (((uint64)mt()) << bits_left);
	bits_left += 32;
      }
    return res;
  }

};

} // namespace __gnu_parallel

#endif
