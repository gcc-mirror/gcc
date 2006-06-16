// random number generation (out of line) -*- C++ -*-

// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <limits>

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE(tr1)

  /*
   * Implementation-space details.
   */
  namespace _Private
  {
    // General case for x = (ax + c) mod m -- use Schrage's algorithm to avoid
    // integer overflow.
    //
    // Because a and c are compile-time integral constants the compiler kindly
    // elides any unreachable paths.
    //
    // Preconditions:  a > 0, m > 0.
    //
    template<typename _Tp, _Tp __a, _Tp __c, _Tp __m, bool>
      struct _Mod
      {
	static _Tp
	__calc(_Tp __x)
	{
	  if (__a == 1)
	    __x %= __m;
	  else
	    {
	      static const _Tp __q = __m / __a;
	      static const _Tp __r = __m % __a;
	      
	      _Tp __t1 = __a * (__x % __q);
	      _Tp __t2 = __r * (__x / __q);
	      if (__t1 >= __t2)
		__x = __t1 - __t2;
	      else
		__x = __m - __t2 + __t1;
	    }

	  if (__c != 0)
	    {
	      const _Tp __d = __m - __x;
	      if (__d > __c)
		__x += __c;
	      else
		__x = __c - __d;
	    }
	  return __x;
	}
      };

    // Special case for m == 0 -- use unsigned integer overflow as modulo
    // operator.
    template<typename _Tp, _Tp __a, _Tp __c, _Tp __m>
      struct _Mod<_Tp, __a, __c, __m, true>
      {
	static _Tp
	__calc(_Tp __x)
	{ return __a * __x + __c; }
      };

    // Dispatch based on modulus value to prevent divide-by-zero compile-time
    // errors when m == 0.
    template<typename _Tp, _Tp __a, _Tp __c, _Tp __m>
      inline _Tp
      __mod(_Tp __x)
      { return _Mod<_Tp, __a, __c, __m, __m == 0>::__calc(__x); }

    // Like the above, for a == 1, c == 0, in terms of w.
    template<typename _Tp, _Tp __w, bool>
      struct _Mod_w
      {
	static _Tp
	__calc(_Tp __x)
	{ return __x % (_Tp(1) << __w); }
      };

    template<typename _Tp, _Tp __w>
      struct _Mod_w<_Tp, __w, true>
      {
	static _Tp
	__calc(_Tp __x)
	{ return __x; }
      };

    template<typename _Tp, _Tp __w>
      inline _Tp
      __mod_w(_Tp __x)
      { return _Mod_w<_Tp, __w,
	              __w == std::numeric_limits<_Tp>::digits>::__calc(__x); }

    // Selector to return the maximum value possible that will fit in 
    // @p __w bits of @p _Tp.
    template<typename _Tp, _Tp __w, bool>
      struct _Max_w
      {
	static _Tp
	__value()
	{ return (_Tp(1) << __w) - 1; }
      };

    template<typename _Tp, _Tp __w>
      struct _Max_w<_Tp, __w, true>
      {
	static _Tp
	__value()
	{ return std::numeric_limits<_Tp>::max(); }
      };

  } // namespace _Private


  /**
   * Constructs the LCR engine with integral seed @p __x0.
   */
  template<class _UIntType, _UIntType __a, _UIntType __c, _UIntType __m>
    linear_congruential<_UIntType, __a, __c, __m>::
    linear_congruential(unsigned long __x0)
    { this->seed(__x0); }

  /**
   * Constructs the LCR engine with seed generated from @p __g.
   */
  template<class _UIntType, _UIntType __a, _UIntType __c, _UIntType __m>
    template<class _Gen>
      linear_congruential<_UIntType, __a, __c, __m>::
      linear_congruential(_Gen& __g)
      { this->seed(__g); }

  /**
   * Seeds the LCR with integral value @p __x0, adjusted so that the 
   * ring identity is never a member of the convergence set.
   */
  template<class _UIntType, _UIntType __a, _UIntType __c, _UIntType __m>
    void
    linear_congruential<_UIntType, __a, __c, __m>::
    seed(unsigned long __x0)
    {
      if ((_Private::__mod<_UIntType, 1, 0, __m>(__c) == 0)
	  && (_Private::__mod<_UIntType, 1, 0, __m>(__x0) == 0))
	_M_x = _Private::__mod<_UIntType, 1, 0, __m>(1);
      else
	_M_x = _Private::__mod<_UIntType, 1, 0, __m>(__x0);
    }

  /**
   * Seeds the LCR engine with a value generated by @p __g.
   */
  template<class _UIntType, _UIntType __a, _UIntType __c, _UIntType __m>
    template<class _Gen>
      void
      linear_congruential<_UIntType, __a, __c, __m>::
      seed(_Gen& __g, false_type)
      {
	_UIntType __x0 = __g();
	if ((_Private::__mod<_UIntType, 1, 0, __m>(__c) == 0)
	    && (_Private::__mod<_UIntType, 1, 0, __m>(__x0) == 0))
	  _M_x = _Private::__mod<_UIntType, 1, 0, __m>(1);
	else
	  _M_x = _Private::__mod<_UIntType, 1, 0, __m>(__x0);
      }

  /**
   * Returns a value that is less than or equal to all values potentially
   * returned by operator(). The return value of this function does not
   * change during the lifetime of the object..
   *
   * The minumum depends on the @p __c parameter: if it is zero, the
   * minimum generated must be > 0, otherwise 0 is allowed.
   */
  template<class _UIntType, _UIntType __a, _UIntType __c, _UIntType __m>
    typename linear_congruential<_UIntType, __a, __c, __m>::result_type
    linear_congruential<_UIntType, __a, __c, __m>::
    min() const
    { return (_Private::__mod<_UIntType, 1, 0, __m>(__c) == 0) ? 1 : 0; }

  /**
   * Gets the maximum possible value of the generated range.
   *
   * For a linear congruential generator, the maximum is always @p __m - 1.
   */
  template<class _UIntType, _UIntType __a, _UIntType __c, _UIntType __m>
    typename linear_congruential<_UIntType, __a, __c, __m>::result_type
    linear_congruential<_UIntType, __a, __c, __m>::
    max() const
    { return (__m == 0) ? std::numeric_limits<_UIntType>::max() : (__m - 1); }

  /**
   * Gets the next generated value in sequence.
   */
  template<class _UIntType, _UIntType __a, _UIntType __c, _UIntType __m>
    typename linear_congruential<_UIntType, __a, __c, __m>::result_type
    linear_congruential<_UIntType, __a, __c, __m>::
    operator()()
    {
      _M_x = _Private::__mod<_UIntType, __a, __c, __m>(_M_x);
      return _M_x;
    }


  template<class _UIntType, int __w, int __n, int __m, int __r,
	   _UIntType __a, int __u, int __s,
	   _UIntType __b, int __t, _UIntType __c, int __l>
    void
    mersenne_twister<_UIntType, __w, __n, __m, __r, __a, __u, __s,
		     __b, __t, __c, __l>::
    seed(unsigned long __value)
    {
      _M_x[0] = _Private::__mod_w<_UIntType, __w>(__value);

      for (int __i = 1; __i < state_size; ++__i)
	{
	  _UIntType __x = _M_x[__i - 1];
	  __x ^= __x >> (__w - 2);
	  __x *= 1812433253ul;
	  __x += __i;
	  _M_x[__i] = _Private::__mod_w<_UIntType, __w>(__x);	  
	}
      _M_p = state_size;
    }

  template<class _UIntType, int __w, int __n, int __m, int __r,
	   _UIntType __a, int __u, int __s,
	   _UIntType __b, int __t, _UIntType __c, int __l>
    template<class _Gen>
      void
      mersenne_twister<_UIntType, __w, __n, __m, __r, __a, __u, __s,
		       __b, __t, __c, __l>::
      seed(_Gen& __gen, false_type)
      {
	for (int __i = 0; __i < state_size; ++__i)
	  _M_x[__i] = _Private::__mod_w<_UIntType, __w>(__gen());
	_M_p = state_size;
      }

  template<class _UIntType, int __w, int __n, int __m, int __r,
	   _UIntType __a, int __u, int __s,
	   _UIntType __b, int __t, _UIntType __c, int __l>
    typename
    mersenne_twister<_UIntType, __w, __n, __m, __r, __a, __u, __s,
		     __b, __t, __c, __l>::result_type
    mersenne_twister<_UIntType, __w, __n, __m, __r, __a, __u, __s,
		     __b, __t, __c, __l>::
    max() const
    {
      using _Private::_Max_w;
      using std::numeric_limits;
      return _Max_w<_UIntType, __w,
	            __w == numeric_limits<_UIntType>::digits>::__value();
    }

  template<class _UIntType, int __w, int __n, int __m, int __r,
	   _UIntType __a, int __u, int __s,
	   _UIntType __b, int __t, _UIntType __c, int __l>
    typename
    mersenne_twister<_UIntType, __w, __n, __m, __r, __a, __u, __s,
		     __b, __t, __c, __l>::result_type
    mersenne_twister<_UIntType, __w, __n, __m, __r, __a, __u, __s,
		     __b, __t, __c, __l>::
    operator()()
    {
      // Reload the vector - cost is O(n) amortized over n calls.
      if (_M_p >= state_size)
	{
	  const _UIntType __upper_mask = (~_UIntType()) << __r;
	  const _UIntType __lower_mask = ~__upper_mask;

	  for (int __k = 0; __k < (__n - __m); ++__k)
	    {
	      _UIntType __y = ((_M_x[__k] & __upper_mask)
			       | (_M_x[__k + 1] & __lower_mask));
	      _M_x[__k] = (_M_x[__k + __m] ^ (__y >> 1)
			   ^ ((__y & 0x01) ? __a : 0));
	    }

	  for (int __k = (__n - __m); __k < (__n - 1); ++__k)
	    {
	      _UIntType __y = ((_M_x[__k] & __upper_mask)
			       | (_M_x[__k + 1] & __lower_mask));
	      _M_x[__k] = (_M_x[__k + (__m - __n)] ^ (__y >> 1)
			   ^ ((__y & 0x01) ? __a : 0));
	    }

	  _UIntType __y = ((_M_x[__n - 1] & __upper_mask)
			   | (_M_x[0] & __lower_mask));
	  _M_x[__n - 1] = (_M_x[__m - 1] ^ (__y >> 1)
			   ^ ((__y & 0x01) ? __a : 0));
	  _M_p = 0;
	}

      // Calculate o(x(i)).
      result_type __z = _M_x[_M_p++];
      __z ^= (__z >> __u);
      __z ^= (__z << __s) & __b;
      __z ^= (__z << __t) & __c;
      __z ^= (__z >> __l);

      return __z;
    }


  template<typename _IntType, _IntType __m, int __s, int __r>
    void
    subtract_with_carry<_IntType, __m, __s, __r>::
    seed(unsigned long __value)
    {
      std::tr1::linear_congruential<unsigned long, 40014, 0, 2147483563>
	__lcg(__value);

      for (int __i = 0; __i < long_lag; ++__i)
	_M_x[__i] = _Private::__mod<_IntType, 1, 0, modulus>(__lcg());

      _M_carry = (_M_x[long_lag - 1] == 0) ? 1 : 0;
      _M_p = 0;
    }

  //
  // This implementation differs from the tr1 spec because the tr1 spec refused
  // to make any sense to me:  the exponent of the factor in the spec goes from
  // 1 to (n-1), but it would only make sense to me if it went from 0 to (n-1).
  //
  // This algorithm is still problematic because it can overflow left right and
  // center.
  //
  template<typename _IntType, _IntType __m, int __s, int __r>
    template<class _Gen>
    void
    subtract_with_carry<_IntType, __m, __s, __r>::
    seed(_Gen& __gen, false_type)
    {
      const int __n = (std::numeric_limits<_IntType>::digits + 31) / 32;
      for (int __i = 0; __i < long_lag; ++__i)
	{
	  _M_x[__i] = 0;
	  unsigned long __factor = 1;
	  for (int __j = 0; __j < __n; ++__j)
	    {
	      _M_x[__i] += __gen() * __factor;
	      __factor *= 0x80000000;
	    }
	  _M_x[__i] = _Private::__mod<_IntType, 1, 0, modulus>(_M_x[__i]);
	}
      _M_carry = (_M_x[long_lag - 1] == 0) ? 1 : 0;
      _M_p = 0;
    }

  template<typename _IntType, _IntType __m, int __s, int __r>
    typename subtract_with_carry<_IntType, __m, __s, __r>::result_type
    subtract_with_carry<_IntType, __m, __s, __r>::
    operator()()
    {
      // Derive short lag index from current index.
      int __ps = _M_p - short_lag;
      if (__ps < 0)
	__ps += long_lag;

      // Calculate new x(i) without overflow or division.
      _IntType __xi;
      if (_M_x[__ps] >= _M_x[_M_p] + _M_carry)
	{
	  __xi = _M_x[__ps] - _M_x[_M_p] - _M_carry;
	  _M_carry = 0;
	}
      else
	{
	  __xi = modulus - _M_x[_M_p] - _M_carry + _M_x[__ps];
	  _M_carry = 1;
	}
      _M_x[_M_p++] = __xi;

      // Adjust current index to loop around in ring buffer.
      if (_M_p >= long_lag)
	_M_p = 0;

      return __xi;
    }


  template<class _UniformRandomNumberGenerator, int __p, int __r>
    typename discard_block<_UniformRandomNumberGenerator,
			   __p, __r>::result_type
    discard_block<_UniformRandomNumberGenerator, __p, __r>::
    operator()()
    {
      if (_M_n >= used_block)
	{
	  while (_M_n < block_size)
	    {
	      _M_b();
	      ++_M_n;
	    }
	  _M_n = 0;
	}
      ++_M_n;
      return _M_b();
    }

_GLIBCXX_END_NAMESPACE
}
