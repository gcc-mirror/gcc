// Random number extensions -*- C++ -*-

// Copyright (C) 2012 Free Software Foundation, Inc.
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

#ifndef _EXT_RANDOM_TCC
#define _EXT_RANDOM_TCC 1

#pragma GCC system_header


namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION


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
    seed(_UIntType __seed)
    {
      _M_state32[0] = static_cast<uint32_t>(__seed);
      for (size_t __i = 1; __i < _M_nstate32; ++__i)
	_M_state32[__i] = (1812433253UL
			   * (_M_state32[__i - 1] ^ (_M_state32[__i - 1] >> 30))
			   + __i);
      _M_pos = state_size;
      _M_period_certification();
    }


  namespace {

    inline uint32_t _Func1(uint32_t __x)
    {
      return (__x ^ (__x >> 27)) * UINT32_C(1664525);
    }

    inline uint32_t _Func2(uint32_t __x)
    {
      return (__x ^ (__x >> 27)) * UINT32_C(1566083941);
    }

  }


  template<typename _UIntType, size_t __m,
	   size_t __pos1, size_t __sl1, size_t __sl2,
	   size_t __sr1, size_t __sr2,
	   uint32_t __msk1, uint32_t __msk2,
	   uint32_t __msk3, uint32_t __msk4,
	   uint32_t __parity1, uint32_t __parity2,
	   uint32_t __parity3, uint32_t __parity4>
    template<typename _Sseq>
      typename std::enable_if<std::is_class<_Sseq>::value>::type
      simd_fast_mersenne_twister_engine<_UIntType, __m,
					__pos1, __sl1, __sl2, __sr1, __sr2,
					__msk1, __msk2, __msk3, __msk4,
					__parity1, __parity2, __parity3,
					__parity4>::
      seed(_Sseq& __q)
      {
	size_t __lag;

	if (_M_nstate32 >= 623)
	  __lag = 11;
	else if (_M_nstate32 >= 68)
	  __lag = 7;
	else if (_M_nstate32 >= 39)
	  __lag = 5;
	else
	  __lag = 3;
	const size_t __mid = (_M_nstate32 - __lag) / 2;

	std::fill(_M_state32, _M_state32 + _M_nstate32, UINT32_C(0x8b8b8b8b));
	uint32_t __arr[_M_nstate32];
	__q.generate(__arr + 0, __arr + _M_nstate32);

	uint32_t __r = _Func1(_M_state32[0] ^ _M_state32[__mid]
			      ^ _M_state32[_M_nstate32  - 1]);
	_M_state32[__mid] += __r;
	__r += _M_nstate32;
	_M_state32[__mid + __lag] += __r;
	_M_state32[0] = __r;

	for (size_t __i = 1, __j = 0; __j < _M_nstate32; ++__j)
	  {
	    __r = _Func1(_M_state32[__i]
			 ^ _M_state32[(__i + __mid) % _M_nstate32]
			 ^ _M_state32[(__i + _M_nstate32 - 1) % _M_nstate32]);
	    _M_state32[(__i + __mid) % _M_nstate32] += __r;
	    __r += __arr[__j] + __i;
	    _M_state32[(__i + __mid + __lag) % _M_nstate32] += __r;
	    _M_state32[__i] = __r;
	    __i = (__i + 1) % _M_nstate32;
	  }
	for (size_t __j = 0; __j < _M_nstate32; ++__j)
	  {
	    const size_t __i = (__j + 1) % _M_nstate32;
	    __r = _Func2(_M_state32[__i]
			 + _M_state32[(__i + __mid) % _M_nstate32]
			 + _M_state32[(__i + _M_nstate32 - 1) % _M_nstate32]);
	    _M_state32[(__i + __mid) % _M_nstate32] ^= __r;
	    __r -= __i;
	    _M_state32[(__i + __mid + __lag) % _M_nstate32] ^= __r;
	    _M_state32[__i] = __r;
	  }

	_M_pos = state_size;
	_M_period_certification();
      }


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
    _M_period_certification(void)
    {
      static const uint32_t __parity[4] = { __parity1, __parity2,
					    __parity3, __parity4 };
      uint32_t __inner = 0;
      for (size_t __i = 0; __i < 4; ++__i)
	if (__parity[__i] != 0)
	  __inner ^= _M_state32[__i] & __parity[__i];

      if (__builtin_parity(__inner) & 1)
	return;
      for (size_t __i = 0; __i < 4; ++__i)
	if (__parity[__i] != 0)
	  {
	    _M_state32[__i] ^= 1 << (__builtin_ffs(__parity[__i]) - 1);
	    return;
	  }
      __builtin_unreachable();
    }


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
    discard(unsigned long long __z)
    {
      while (__z > state_size - _M_pos)
	{
	  __z -= state_size - _M_pos;

	  _M_gen_rand();
	}

      _M_pos += __z;
    }


#ifdef  __SSE2__

  namespace {

    template<size_t __sl1, size_t __sl2, size_t __sr1, size_t __sr2,
	     uint32_t __msk1, uint32_t __msk2, uint32_t __msk3, uint32_t __msk4>
      inline __m128i __sse2_recursion(__m128i __a, __m128i __b,
				      __m128i __c, __m128i __d)
      {
	__m128i __y = _mm_srli_epi32(__b, __sr1);
	__m128i __z = _mm_srli_si128(__c, __sr2);
	__m128i __v = _mm_slli_epi32(__d, __sl1);
	__z = _mm_xor_si128(__z, __a);
	__z = _mm_xor_si128(__z, __v);
	__m128i __x = _mm_slli_si128(__a, __sl2);
	__y = _mm_and_si128(__y, _mm_set_epi32(__msk4, __msk3, __msk2, __msk1));
	__z = _mm_xor_si128(__z, __x);
	return _mm_xor_si128(__z, __y);
      }

  }


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
    _M_gen_rand(void)
    {
      __m128i __r1 = _mm_load_si128(&_M_state[_M_nstate - 2]);
      __m128i __r2 = _mm_load_si128(&_M_state[_M_nstate - 1]);

      size_t __i;
      for (__i = 0; __i < _M_nstate - __pos1; ++__i)
	{
	  __m128i __r = __sse2_recursion<__sl1, __sl2, __sr1, __sr2,
					 __msk1, __msk2, __msk3, __msk4>
	    (_M_state[__i], _M_state[__i + __pos1], __r1, __r2);
	  _mm_store_si128(&_M_state[__i], __r);
	  __r1 = __r2;
	  __r2 = __r;
	}
      for (; __i < _M_nstate; ++__i)
	{
	  __m128i __r = __sse2_recursion<__sl1, __sl2, __sr1, __sr2,
					 __msk1, __msk2, __msk3, __msk4>
	    (_M_state[__i], _M_state[__i + __pos1 - _M_nstate], __r1, __r2);
	  _mm_store_si128(&_M_state[__i], __r);
	  __r1 = __r2;
	  __r2 = __r;
	}

      _M_pos = 0;
    }


#else

  namespace {

    template<size_t __shift>
      inline void __rshift(uint32_t *__out, const uint32_t *__in)
      {
	uint64_t __th = ((static_cast<uint64_t>(__in[3]) << 32)
			 | static_cast<uint64_t>(__in[2]));
	uint64_t __tl = ((static_cast<uint64_t>(__in[1]) << 32)
			 | static_cast<uint64_t>(__in[0]));

	uint64_t __oh = __th >> (__shift * 8);
	uint64_t __ol = __tl >> (__shift * 8);
	__ol |= __th << (64 - __shift * 8);
	__out[1] = static_cast<uint32_t>(__ol >> 32);
	__out[0] = static_cast<uint32_t>(__ol);
	__out[3] = static_cast<uint32_t>(__oh >> 32);
	__out[2] = static_cast<uint32_t>(__oh);
      }


    template<size_t __shift>
      inline void __lshift(uint32_t *__out, const uint32_t *__in)
      {
	uint64_t __th = ((static_cast<uint64_t>(__in[3]) << 32)
			 | static_cast<uint64_t>(__in[2]));
	uint64_t __tl = ((static_cast<uint64_t>(__in[1]) << 32)
			 | static_cast<uint64_t>(__in[0]));

	uint64_t __oh = __th << (__shift * 8);
	uint64_t __ol = __tl << (__shift * 8);
	__oh |= __tl >> (64 - __shift * 8);
	__out[1] = static_cast<uint32_t>(__ol >> 32);
	__out[0] = static_cast<uint32_t>(__ol);
	__out[3] = static_cast<uint32_t>(__oh >> 32);
	__out[2] = static_cast<uint32_t>(__oh);
      }


    template<size_t __sl1, size_t __sl2, size_t __sr1, size_t __sr2,
	     uint32_t __msk1, uint32_t __msk2, uint32_t __msk3, uint32_t __msk4>
      inline void __recursion(uint32_t *__r,
			      const uint32_t *__a, const uint32_t *__b,
			      const uint32_t *__c, const uint32_t *__d)
      {
	uint32_t __x[4];
	uint32_t __y[4];

	__lshift<__sl2>(__x, __a);
	__rshift<__sr2>(__y, __c);
	__r[0] = (__a[0] ^ __x[0] ^ ((__b[0] >> __sr1) & __msk1)
		  ^ __y[0] ^ (__d[0] << __sl1));
	__r[1] = (__a[1] ^ __x[1] ^ ((__b[1] >> __sr1) & __msk2)
		  ^ __y[1] ^ (__d[1] << __sl1));
	__r[2] = (__a[2] ^ __x[2] ^ ((__b[2] >> __sr1) & __msk3)
		  ^ __y[2] ^ (__d[2] << __sl1));
	__r[3] = (__a[3] ^ __x[3] ^ ((__b[3] >> __sr1) & __msk4)
		  ^ __y[3] ^ (__d[3] << __sl1));
      }

  }


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
    _M_gen_rand(void)
    {
      const uint32_t *__r1 = &_M_state32[_M_nstate32 - 8];
      const uint32_t *__r2 = &_M_state32[_M_nstate32 - 4];
      static constexpr size_t __pos1_32 = __pos1 * 4;

      size_t __i;
      for (__i = 0; __i < _M_nstate32 - __pos1_32; __i += 4)
	{
	  __recursion<__sl1, __sl2, __sr1, __sr2,
		      __msk1, __msk2, __msk3, __msk4>
	    (&_M_state32[__i], &_M_state32[__i],
	     &_M_state32[__i + __pos1_32], __r1, __r2);
	  __r1 = __r2;
	  __r2 = &_M_state32[__i];
	}

      for (; __i < _M_nstate32; __i += 4)
	{
	  __recursion<__sl1, __sl2, __sr1, __sr2,
		      __msk1, __msk2, __msk3, __msk4>
	    (&_M_state32[__i], &_M_state32[__i],
	     &_M_state32[__i + __pos1_32 - _M_nstate32], __r1, __r2);
	  __r1 = __r2;
	  __r2 = &_M_state32[__i];
	}

      _M_pos = 0;
    }

#endif


  template<typename _UIntType, size_t __m,
	   size_t __pos1, size_t __sl1, size_t __sl2,
	   size_t __sr1, size_t __sr2,
	   uint32_t __msk1, uint32_t __msk2,
	   uint32_t __msk3, uint32_t __msk4,
	   uint32_t __parity1, uint32_t __parity2,
	   uint32_t __parity3, uint32_t __parity4,
	   typename _CharT, typename _Traits>
    std::basic_ostream<_CharT, _Traits>&
    operator<<(std::basic_ostream<_CharT, _Traits>& __os,
	       const __gnu_cxx::simd_fast_mersenne_twister_engine<_UIntType,
	       __m, __pos1, __sl1, __sl2, __sr1, __sr2,
	       __msk1, __msk2, __msk3, __msk4,
	       __parity1, __parity2, __parity3, __parity4>& __x)
    {
      typedef std::basic_ostream<_CharT, _Traits> __ostream_type;
      typedef typename __ostream_type::ios_base __ios_base;

      const typename __ios_base::fmtflags __flags = __os.flags();
      const _CharT __fill = __os.fill();
      const _CharT __space = __os.widen(' ');
      __os.flags(__ios_base::dec | __ios_base::fixed | __ios_base::left);
      __os.fill(__space);

      for (size_t __i = 0; __i < __x._M_nstate32; ++__i)
	__os << __x._M_state32[__i] << __space;
      __os << __x._M_pos;

      __os.flags(__flags);
      __os.fill(__fill);
      return __os;
    }


  template<typename _UIntType, size_t __m,
	   size_t __pos1, size_t __sl1, size_t __sl2,
	   size_t __sr1, size_t __sr2,
	   uint32_t __msk1, uint32_t __msk2,
	   uint32_t __msk3, uint32_t __msk4,
	   uint32_t __parity1, uint32_t __parity2,
	   uint32_t __parity3, uint32_t __parity4,
	   typename _CharT, typename _Traits>
    std::basic_istream<_CharT, _Traits>&
    operator>>(std::basic_istream<_CharT, _Traits>& __is,
	       __gnu_cxx::simd_fast_mersenne_twister_engine<_UIntType,
	       __m, __pos1, __sl1, __sl2, __sr1, __sr2,
	       __msk1, __msk2, __msk3, __msk4,
	       __parity1, __parity2, __parity3, __parity4>& __x)
    {
      typedef std::basic_istream<_CharT, _Traits> __istream_type;
      typedef typename __istream_type::ios_base __ios_base;

      const typename __ios_base::fmtflags __flags = __is.flags();
      __is.flags(__ios_base::dec | __ios_base::skipws);

      for (size_t __i = 0; __i < __x._M_nstate32; ++__i)
	__is >> __x._M_state32[__i];
      __is >> __x._M_pos;

      __is.flags(__flags);
      return __is;
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace


#endif // _EXT_RANDOM_TCC
