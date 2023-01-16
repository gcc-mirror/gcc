// Optimizations for random number extensions, aarch64 version -*- C++ -*-

// Copyright (C) 2017-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file ext/random.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{ext/random}
 */

#ifndef _EXT_OPT_RANDOM_H
#define _EXT_OPT_RANDOM_H 1

#pragma GCC system_header

#ifdef __ARM_NEON

#ifdef __ARM_BIG_ENDIAN
# define __VEXT(_A,_B,_C) __builtin_shuffle (_A, _B, (__Uint8x16_t) \
    {16-_C, 17-_C, 18-_C, 19-_C, 20-_C, 21-_C, 22-_C, 23-_C, \
     24-_C, 25-_C, 26-_C, 27-_C, 28-_C, 29-_C, 30-_C, 31-_C})
#else
# define __VEXT(_A,_B,_C) __builtin_shuffle (_B, _A, (__Uint8x16_t) \
    {_C, _C+1, _C+2, _C+3, _C+4, _C+5, _C+6, _C+7, \
     _C+8, _C+9, _C+10, _C+11, _C+12, _C+13, _C+14, _C+15})
#endif

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
namespace __gnu_cxx _GLIBCXX_VISIBILITY (default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  namespace {
    // Logical Shift right 128-bits by c * 8 bits

    __extension__ extern __inline __Uint32x4_t
    __attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
    __aarch64_lsr_128 (__Uint8x16_t __a, __const int __c)
    {
      const __Uint8x16_t __zero = {0, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 0};

      return (__Uint32x4_t) __VEXT (__zero, __a, __c);
    }

    // Logical Shift left 128-bits by c * 8 bits

    __extension__ extern __inline __Uint32x4_t
    __attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
    __aarch64_lsl_128 (__Uint8x16_t __a, __const int __c)
    {
      const __Uint8x16_t __zero = {0, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 0};

      return (__Uint32x4_t) __VEXT (__a, __zero, 16 - __c);
    }

    template<size_t __sl1, size_t __sl2, size_t __sr1, size_t __sr2>
      inline __Uint32x4_t __aarch64_recursion (__Uint32x4_t __a,
					       __Uint32x4_t __b,
					       __Uint32x4_t __c,
					       __Uint32x4_t __d,
					       __Uint32x4_t __e)
    {
      __Uint32x4_t __y = (__b >> __sr1);
      __Uint32x4_t __z = __aarch64_lsr_128 ((__Uint8x16_t) __c, __sr2);

      __Uint32x4_t __v = __d << __sl1;

      __z = __z ^ __a;
      __z = __z ^ __v;

      __Uint32x4_t __x = __aarch64_lsl_128 ((__Uint8x16_t) __a, __sl2);

      __y = __y & __e;
      __z = __z ^ __x;
      return __z ^ __y;
    }
}

#define _GLIBCXX_OPT_HAVE_RANDOM_SFMT_GEN_READ	1
  template<typename _UIntType, size_t __m,
	   size_t __pos1, size_t __sl1, size_t __sl2,
	   size_t __sr1, size_t __sr2,
	   uint32_t __msk1, uint32_t __msk2,
	   uint32_t __msk3, uint32_t __msk4,
	   uint32_t __parity1, uint32_t __parity2,
	   uint32_t __parity3, uint32_t __parity4>
    void simd_fast_mersenne_twister_engine<_UIntType, __m,
					   __pos1, __sl1, __sl2, __sr1, __sr2,
					   __msk1, __msk2, __msk3, __msk4,
					   __parity1, __parity2, __parity3,
					   __parity4>::
    _M_gen_rand (void)
    {
      __Uint32x4_t __r1 = _M_state[_M_nstate - 2];
      __Uint32x4_t __r2 = _M_state[_M_nstate - 1];

      __Uint32x4_t __aData = {__msk1, __msk2, __msk3, __msk4};

      size_t __i;
      for (__i = 0; __i < _M_nstate - __pos1; ++__i)
	{
	  __Uint32x4_t __r = __aarch64_recursion<__sl1, __sl2, __sr1, __sr2>
	    (_M_state[__i], _M_state[__i + __pos1], __r1, __r2, __aData);

	  _M_state[__i] = __r;

	  __r1 = __r2;
	  __r2 = __r;
	}
      for (; __i < _M_nstate; ++__i)
	{
	  __Uint32x4_t __r = __aarch64_recursion<__sl1, __sl2, __sr1, __sr2>
	    (_M_state[__i], _M_state[__i + __pos1 - _M_nstate], __r1, __r2,
	     __aData);

	  _M_state[__i] = __r;

	  __r1 = __r2;
	  __r2 = __r;
	}

      _M_pos = 0;
    }


#define _GLIBCXX_OPT_HAVE_RANDOM_SFMT_OPERATOREQUAL	1
  template<typename _UIntType, size_t __m,
	   size_t __pos1, size_t __sl1, size_t __sl2,
	   size_t __sr1, size_t __sr2,
	   uint32_t __msk1, uint32_t __msk2,
	   uint32_t __msk3, uint32_t __msk4,
	   uint32_t __parity1, uint32_t __parity2,
	   uint32_t __parity3, uint32_t __parity4>
    bool
    operator==(const __gnu_cxx::simd_fast_mersenne_twister_engine<_UIntType,
	       __m, __pos1, __sl1, __sl2, __sr1, __sr2,
	       __msk1, __msk2, __msk3, __msk4,
	       __parity1, __parity2, __parity3, __parity4>& __lhs,
	       const __gnu_cxx::simd_fast_mersenne_twister_engine<_UIntType,
	       __m, __pos1, __sl1, __sl2, __sr1, __sr2,
	       __msk1, __msk2, __msk3, __msk4,
	       __parity1, __parity2, __parity3, __parity4>& __rhs)
    {
      if (__lhs._M_pos != __rhs._M_pos)
	return false;

      __Uint32x4_t __res = __lhs._M_state[0] ^ __rhs._M_state[0];

      for (size_t __i = 1; __i < __lhs._M_nstate; ++__i)
	__res |= __lhs._M_state[__i] ^ __rhs._M_state[__i];

      return (__int128) __res == 0;
    }

_GLIBCXX_END_NAMESPACE_VERSION
  } // namespace

#endif // __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#endif // __ARM_NEON

#endif // _EXT_OPT_RANDOM_H
